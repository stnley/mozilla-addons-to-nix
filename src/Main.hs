{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Lens
import           GHC.IO.Encoding              (setLocaleEncoding, utf8)
import           Paths_nixpkgs_firefox_addons
import           System.Directory             (copyFile)
import           System.Environment           (getArgs)
import           System.Nixpkgs.FirefoxAddons

addonSlugs :: [AddonReq]
addonSlugs = [ "cookie-autodelete"
             , "decentraleyes"
             , "greasemonkey"
             , AddonReq "görans-hemmasnickrade-ordli"
                        (addonSlug .~ "swedish-dictionary")
             , "https-everywhere"
             , "link-cleaner"
             , "octotree"
             , AddonReq "privacy-badger17"
                        (addonSlug .~ "privacy-badger")
             , "reddit-enhancement-suite"
             , "save-page-we"
             , AddonReq "styl-us"
                        (addonSlug .~ "stylus")
             , "ublock-origin"
             ]

parseArgs :: IO FilePath
parseArgs = getArgs >>= parse
  where
    usage =
      do
        putTextLn "Expects a single argument with the output directory"
        exitSuccess

    parse ["--help"]  = usage
    parse [outputDir] = return outputDir
    parse _           = usage

main :: IO ()
main =
  do
    setLocaleEncoding utf8

    outDir <- parseArgs
    generatedCode <- generateFirefoxAddonPackages addonSlugs
    putTextLn "Writing generated-firefox-addons.nix"
    writeFile (outDir <> "/generated-firefox-addons.nix") generatedCode
    putTextLn "Writing default.nix"
    mainNixFile <- getDataFileName "firefox-addons.nix"
    copyFile mainNixFile (outDir <> "/default.nix")
