{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Main
Copyright: (c) 2025 Jose Storopoli
License: MIT
Maintainer: Jose Storopoli <jose@storopoli.com>

CLI entry point for the Bitcoin vanity address generator.
-}
module Main (main) where

import Control.Monad (unless, when)
import Crypto.Bitcoin.Vanity (
  Address (..),
  AddressType (..),
  Backend (..),
  Network (..),
  Pattern (..),
  SecretKey (..),
  VanityConfig (..),
  VanityResult (..),
  availableBackends,
  searchWithProgress,
  toWIF,
 )
import Crypto.Bitcoin.Vanity.Key (integerToBytes32)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | CLI options.
data Options = Options
  { optPattern :: Text
  , optNetwork :: Network
  , optAddressType :: AddressType
  , optBackend :: Backend
  , optThreads :: Maybe Int
  , optCaseInsensitive :: Bool
  , optQuiet :: Bool
  , optShowPrivate :: Bool
  }
  deriving (Show)

-- | Parse CLI options.
optionsParser :: Parser Options
optionsParser = do
  optPattern <-
    strArgument
      ( metavar "PATTERN"
          <> help "Regex pattern to match in the address"
      )

  optNetwork <-
    option
      parseNetwork
      ( long "network"
          <> short 'n'
          <> metavar "NETWORK"
          <> value Mainnet
          <> showDefaultWith showNetwork
          <> help "Bitcoin network: mainnet or testnet"
      )

  optAddressType <-
    option
      parseAddressType
      ( long "type"
          <> short 't'
          <> metavar "TYPE"
          <> value P2PKH
          <> showDefaultWith showAddressType
          <> help "Address type: p2pkh, p2wpkh, or p2tr"
      )

  optBackend <-
    option
      parseBackend
      ( long "backend"
          <> short 'b'
          <> metavar "BACKEND"
          <> value CPU
          <> showDefaultWith showBackend
          <> help "Computation backend: cpu, cuda, or wgpu"
      )

  optThreads <-
    optional $
      option
        auto
        ( long "threads"
            <> short 'j'
            <> metavar "N"
            <> help "Number of threads (default: all cores)"
        )

  optCaseInsensitive <-
    switch
      ( long "ignore-case"
          <> short 'i'
          <> help "Case-insensitive pattern matching"
      )

  optQuiet <-
    switch
      ( long "quiet"
          <> short 'q'
          <> help "Suppress progress output"
      )

  optShowPrivate <-
    switch
      ( long "show-private"
          <> help "Display the private key (WIF and hex) in output"
      )

  pure Options{..}

-- | Network parser.
parseNetwork :: ReadM Network
parseNetwork = eitherReader $ \case
  "mainnet" -> Right Mainnet
  "testnet" -> Right Testnet
  _ -> Left "Invalid network. Use 'mainnet' or 'testnet'."

-- | Show network for help text.
showNetwork :: Network -> String
showNetwork Mainnet = "mainnet"
showNetwork Testnet = "testnet"

-- | Address type parser.
parseAddressType :: ReadM AddressType
parseAddressType = eitherReader $ \case
  "p2pkh" -> Right P2PKH
  "p2wpkh" -> Right P2WPKH
  "p2tr" -> Right P2TR
  "legacy" -> Right P2PKH
  "segwit" -> Right P2WPKH
  "taproot" -> Right P2TR
  _ -> Left "Invalid address type. Use 'p2pkh', 'p2wpkh', or 'p2tr'."

-- | Show address type for help text.
showAddressType :: AddressType -> String
showAddressType P2PKH = "p2pkh"
showAddressType P2WPKH = "p2wpkh"
showAddressType P2TR = "p2tr"

-- | Backend parser.
parseBackend :: ReadM Backend
parseBackend = eitherReader $ \case
  "cpu" -> Right CPU
  "cuda" -> Right CUDA
  "wgpu" -> Right WGPU
  "gpu" -> Right CUDA -- Default GPU to CUDA
  _ -> Left "Invalid backend. Use 'cpu', 'cuda', or 'wgpu'."

-- | Show backend for help text.
showBackend :: Backend -> String
showBackend CPU = "cpu"
showBackend CUDA = "cuda"
showBackend WGPU = "wgpu"

-- | Program info.
programInfo :: ParserInfo Options
programInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Generate Bitcoin vanity addresses matching a regex pattern"
        <> header "vanity - Bitcoin vanity address generator"
        <> footer ("Available backends: " ++ show availableBackends)
    )

-- | Main entry point.
main :: IO ()
main = do
  Options{..} <- execParser programInfo

  -- Validate backend availability
  unless (optBackend `elem` availableBackends) $
    do
      putStrLn $
        "Error: Backend " ++ show optBackend ++ " is not available."
      putStrLn $ "Available backends: " ++ show availableBackends
      exitFailure

  -- Build configuration
  let config =
        VanityConfig
          { vcNetwork = optNetwork
          , vcAddressType = optAddressType
          , vcPattern = Pattern optPattern
          , vcBackend = optBackend
          , vcThreads = optThreads
          , vcCaseSensitive = not optCaseInsensitive
          }

  -- Progress callback
  let progressCallback :: Word64 -> IO ()
      progressCallback attempts =
        if optQuiet
          then pure ()
          else do
            printf "\rAttempts: %d" attempts
            hFlush stdout

  -- Print search info
  if optQuiet
    then pure ()
    else do
      putStrLn $ "Searching for pattern: " ++ T.unpack optPattern
      putStrLn $ "Network: " ++ showNetwork optNetwork
      putStrLn $ "Address type: " ++ showAddressType optAddressType
      putStrLn $ "Case sensitive: " ++ show (not optCaseInsensitive)
      putStrLn ""

  -- Run search
  result <- searchWithProgress config progressCallback

  case result of
    Left err -> do
      if optQuiet then pure () else putStrLn ""
      putStrLn $ "Error: " ++ show err
      exitFailure
    Right VanityResult{..} -> do
      if optQuiet then pure () else putStrLn "" -- Newline after progress
      putStrLn ""
      putStrLn "=== FOUND ==="
      TIO.putStrLn $ "Address: " <> unAddress vrAddress
      putStrLn $ "Attempts: " ++ show vrAttempts

      when optShowPrivate $
        do
          putStrLn ""
          putStrLn "=== PRIVATE KEY (KEEP SECRET!) ==="
          BS8.putStrLn $ "WIF: " <> toWIF optNetwork vrSecretKey
          putStrLn $ "Hex: " ++ showSecretKeyHex vrSecretKey

      exitSuccess

-- | Show secret key as hex string.
showSecretKeyHex :: SecretKey -> String
showSecretKeyHex (SecretKey sk) = concatMap (printf "%02x") (BS8.unpack $ integerToBytes32 sk)
