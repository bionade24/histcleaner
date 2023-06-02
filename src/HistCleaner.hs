module HistCleaner
  ( storeSecret
  , removeSecret
  , SStorageResult(..)
  , cleanFile
  , CleanResult(..)
  )
  where

import HistCleaner.Cleaner
import HistCleaner.SecretStorage
