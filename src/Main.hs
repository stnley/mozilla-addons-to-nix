{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson
import Data.Char (isUpper)
import qualified Data.Text as T
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lens.Micro.Platform
import qualified Relude.Unsafe as Unsafe
import System.Environment (getProgName)
import qualified System.Nixpkgs.FirefoxAddons as FA

newtype AddonLicense = AddonLicense {getAddonLicense :: FA.AddonLicense}
  deriving (Show, Generic)

instance FromJSON AddonLicense where
  parseJSON =
    let toLowerInit (h, t) = T.toLower h <> t
        opts =
          defaultOptions
            { constructorTagModifier =
                toString
                  . T.toLower
                  . Unsafe.fromJust
                  . T.stripPrefix "AddonLicense"
                  . toText,
              fieldLabelModifier =
                toString
                  . toLowerInit
                  . T.span isUpper
                  . Unsafe.fromJust
                  . T.stripPrefix "addonLicense"
                  . toText
            }
     in fmap AddonLicense . genericParseJSON opts

data Addon = Addon
  { slug :: Text,
    pname :: Maybe Text,
    license :: Maybe AddonLicense
  }
  deriving (Show, Generic)

instance FromJSON Addon

data CmdArgs = CmdArgs
  { inputFile :: FilePath,
    outputFile :: FilePath
  }
  deriving (Eq, Show)

parseArgs :: IO CmdArgs
parseArgs = getArgs >>= parse
  where
    usage =
      do
        progName <- getProgName
        putTextLn $ toText progName <> " IN_FILE OUT_FILE"
        putTextLn ""
        putTextLn "where IN_FILE is a JSON file containing a list of addons"
        putTextLn "and OUT_FILE is the Nix expression destination."
        exitSuccess

    parse ["--help"] = usage
    parse [jsonFile, nixFile] = pure $ CmdArgs jsonFile nixFile
    parse _ = usage

fetchAddons :: FilePath -> [Addon] -> IO ()
fetchAddons outputFile addons = addonsExpr >>= writeFileText outputFile
  where
    addonsExpr = FA.generateFirefoxAddonPackages . map mkAddonReq $ addons
    mkAddonReq Addon {..} =
      FA.AddonReq
        slug
        ( maybe id (set FA.addonLicense . Just . getAddonLicense) license
            . maybe id (FA.addonNixName .~) pname
        )

printAndPanic :: String -> IO ()
printAndPanic t = putStrLn t >> exitFailure

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    CmdArgs {..} <- parseArgs

    decoded <- eitherDecodeFileStrict' inputFile
    either printAndPanic (fetchAddons outputFile) decoded
