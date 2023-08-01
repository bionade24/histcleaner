{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import System.Exit
import System.IO

import HistCleaner
import OptParse

main :: IO ()
main = do
  command <- parse
  case command of
    ManageSecrets subcmd ->
      case subcmd of
        AddSecret -> do
          putStrLn "Enter the secret you want to add: "
          secret <- withEcho False C8.getLine
          storeSecret secret >>= handleSecretStorage
        RemoveSecret -> do
          putStrLn "Enter the secret you want to remove: "
          secret <- withEcho False C8.getLine
          removeSecret secret >>= handleSecretStorage
    CleanFile args -> do
      case args of
        Files f -> do
          for_ f $ cleanFile >=> handleCleaner
          exitSuccess

handleSecretStorage :: SStorageResult -> IO ()
handleSecretStorage result = do
  case result of
    SSuccess -> do
      exitSuccess
    SHashFail -> do
      putStrLn "Hashing of the input failed"
    StoreFail msg -> do
      putStrLn $ "Storing the secret failed:\n" <> msg
    NoMatch -> do
      putStrLn "No matching secret in the database"
    RemoveFail msg -> do
      putStrLn $ "Removing the secret failed:\n" <> msg
  exitFailure

handleCleaner :: CleanResult -> IO ()
handleCleaner result = do
  case result of
    CSuccess -> do
      pure ()
    CHashFail input -> do
      putStrLn $ "Hashing of the input failed: " <> input
    -- TODO: Catch Read/Write errors and use this
    ReadFail path -> do
      putStrLn $ "Reading of file failed: " <> path
    WriteFail path -> do
      putStrLn $ "Writing of file failed: " <> path
  if result /= CSuccess
    then exitFailure
    else do
      pure ()

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
