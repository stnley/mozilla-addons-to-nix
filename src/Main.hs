{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens
import Data.Aeson
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs, getProgName)
import System.Nixpkgs.FirefoxAddons

data Addon = Addon { slug :: Text, pname :: Maybe Text }
  deriving (Show, Generic)

instance FromJSON Addon

data CmdArgs = CmdArgs { inputFile :: FilePath
                       , outputFile :: FilePath
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
    parse _          = usage

fetchAddons :: FilePath -> [Addon] -> IO ()
fetchAddons outputFile addons = addonsExpr >>= writeFileText outputFile
  where
    addonsExpr = generateFirefoxAddonPackages . map mkAddonReq $ addons
    mkAddonReq Addon {..} = AddonReq slug (maybe id (addonNixName .~) pname)

printAndPanic :: String -> IO ()
printAndPanic t = putStrLn t >> exitFailure

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    CmdArgs {..} <- parseArgs

    decoded <- eitherDecodeFileStrict' inputFile
    either printAndPanic (fetchAddons outputFile) decoded
