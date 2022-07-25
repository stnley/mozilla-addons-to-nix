{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module System.Nixpkgs.FirefoxAddons
  ( AddonReq (..),
    AddonData,
    AddonFile,
    AddonLicense (..),
    Hash,
    generateFirefoxAddonPackages,
    addonDescription,
    addonFile,
    addonHomepage,
    addonId,
    addonLicense,
    addonLicenseId,
    addonNixName,
    addonSlug,
    addonVersion,
  )
where

import Data.Aeson
import qualified Data.Text as T
import Lens.Micro.Platform
import Network.Wreq (asJSON, responseBody)
import qualified Network.Wreq.Session as Wreq
import Nix.Expr
import Nix.Pretty

data HashType = Sha256
  deriving (Eq, Show)

data AddonReq = AddonReq
  { _addonReqSlug :: Text,
    _addonReqModify :: AddonData -> AddonData
  }

data Hash = Hash
  { _hashValue :: Text,
    _hashType :: HashType
  }
  deriving (Eq, Show)

data AddonFile = AddonFile
  { _addonFileUrl :: Text,
    _addonFileHash :: Hash
  }
  deriving (Eq, Show)

data AddonLicense
  = AddonLicensePredefined {addonLicenseShortName :: Text}
  | AddonLicenseCustom
      { addonLicenseShortName :: Text,
        addonLicenseFullName :: Text,
        addonLicenseUrl :: Text,
        addonLicenseFree :: Bool
      }
  deriving (Eq, Generic, Show)

data AddonData = AddonData
  { _addonId :: Text,
    _addonSlug :: Text,
    _addonNixName :: Text,
    _addonVersion :: Text,
    _addonFile :: AddonFile,
    _addonDescription :: Text,
    _addonLicenseId :: Int,
    _addonLicense :: Maybe AddonLicense,
    _addonHomepage :: Maybe Text
  }
  deriving (Eq, Show)

makeLenses ''Hash
makeLenses ''AddonFile
makeLenses ''AddonLicense
makeLenses ''AddonData

instance IsString AddonReq where
  fromString name = AddonReq (fromString name) id

instance FromJSON AddonFile where
  parseJSON = withObject "AddonFile" $ \obj ->
    do
      _addonFileUrl <- obj .: "url"
      _addonFileHash <- parseHash =<< obj .: "hash"
      return AddonFile {..}

instance FromJSON AddonData where
  parseJSON = withObject "AddonData" $ \obj ->
    do
      _addonId <- obj .: "guid"
      _addonSlug <- obj .: "slug"
      let _addonNixName = _addonSlug
      _addonDescription <- obj .: "summary" <|> (obj .: "summary" >>= (.: "en-US"))
      _addonHomepage <- obj .: "homepage" <|> (obj .: "homepage" >>= (.: "en-US"))

      currentVersionObj <- obj .: "current_version"
      _addonVersion <- currentVersionObj .: "version"

      licenseObj <- currentVersionObj .: "license"
      _addonLicenseId <- licenseObj .: "id"
      let _addonLicense = licenses ^. at _addonLicenseId

      addonFiles :: [AddonFile] <- currentVersionObj .: "files"
      _addonFile <-
        maybe
          (fail "No file to download")
          (return . head)
          (nonEmpty addonFiles)

      return AddonData {..}

parseHash :: MonadFail m => Text -> m Hash
parseHash raw =
  case T.splitOn ":" raw of
    ["sha256", hash] -> pure $ Hash hash Sha256
    _ -> fail . toString $ "Unknown hash: " <> raw

licenses :: IntMap AddonLicense
licenses =
  fromList
    [ (6, AddonLicensePredefined "gpl3"),
      (12, AddonLicensePredefined "lgpl3"),
      (13, AddonLicensePredefined "gpl2"),
      (16, AddonLicensePredefined "lgpl21"),
      (22, AddonLicensePredefined "mit"),
      (3338, AddonLicensePredefined "mpl20"),
      (4160, AddonLicensePredefined "asl20"),
      (4814, AddonLicensePredefined "cc0"),
      (5296, AddonLicensePredefined "bsd3"),
      (6978, AddonLicensePredefined "cc-by-sa-30"),
      (6979, AddonLicensePredefined "cc-by-30"),
      (6982, AddonLicensePredefined "cc-by-nc-sa-30"),
      (7551, AddonLicensePredefined "isc"),
      (7770, AddonLicensePredefined "agpl3")
    ]

license :: AddonLicense -> NExpr
license AddonLicensePredefined {..} =
  mkSym ("licenses." <> addonLicenseShortName)
license AddonLicenseCustom {..} =
  attrsE
    [ ("shortName", mkStr addonLicenseShortName),
      ("fullName", mkStr addonLicenseFullName),
      ("url", mkStr addonLicenseUrl),
      ("free", mkBool addonLicenseFree)
    ]

-- | Builds a Nix derivation for the given add-on.
addonDrv :: AddonData -> NExpr
addonDrv addon = "buildFirefoxXpiAddon" @@ fields
  where
    fields =
      attrsE
        [ ("pname", mkStr $ addon ^. addonNixName),
          ("version", mkStr $ addon ^. addonVersion),
          ("addonId", mkStr $ addon ^. addonId),
          ("url", mkStr $ file ^. addonFileUrl),
          (hashAttrName, mkStr $ file ^. addonFileHash . hashValue),
          ("meta", meta)
        ]

    file = addon ^. addonFile

    hashAttrName =
      case file ^. addonFileHash . hashType of
        Sha256 -> "sha256"

    optAttr n = toList . fmap (n,)

    meta =
      mkWith "lib" $
        attrsE $
          optAttr "homepage" (mkStr <$> addon ^. addonHomepage)
            <> [("description", mkStr $ addon ^. addonDescription)]
            <> optAttr "license" (license <$> addon ^. addonLicense)
            <> [("platforms", "platforms.all")]

addonDrvs :: [AddonData] -> NExpr
addonDrvs = attrsE . map attr
  where
    quoted s = "\"" <> s <> "\""
    attr addon = (quoted $ addon ^. addonNixName, addonDrv addon)

packageFun :: [AddonData] -> NExpr
packageFun addons =
  mkParamset params False ==> addonDrvs addons
  where
    params =
      [ ("buildFirefoxXpiAddon", Nothing),
        ("fetchurl", Nothing),
        ("lib", Nothing),
        ("stdenv", Nothing)
      ]

addonsApiBase :: Text
addonsApiBase = "https://addons.mozilla.org/api/v4"

addonUrl :: Text -> Text
addonUrl slug =
  addonsApiBase
    <> "/addons/addon/"
    <> slug
    <> "/?app=firefox&lang=en-US"

fetchAddonData :: Wreq.Session -> Text -> IO AddonData
fetchAddonData sess slug =
  do
    addon <- view responseBody <$> fetch
    putTextLn $ "Fetched " <> slug <> " version " <> addon ^. addonVersion
    return addon
  where
    fetch = Wreq.get sess (toString $ addonUrl slug) >>= asJSON

generateFirefoxAddonPackages :: [AddonReq] -> IO Text
generateFirefoxAddonPackages reqs =
  do
    sess <- Wreq.newAPISession
    addons <- sortOn (^. addonNixName) <$> mapM (fetchAndModify sess) reqs
    pure . show . prettyNix . packageFun $ addons
  where
    fetchAndModify sess AddonReq {..} =
      _addonReqModify <$> fetchAddonData sess _addonReqSlug
