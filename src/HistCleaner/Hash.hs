module HistCleaner.Hash
  ( hash
  , newSalt
  ) where

import Crypto.Error
import Crypto.KDF.Argon2
  ( Variant(..)
  , Version(..)
  , iterations
  , memory
  , parallelism
  , variant
  , version
  )
import qualified Crypto.KDF.Argon2 as Argon2
import Crypto.Random
import Data.ByteString (ByteString)

hash :: ByteString -> ByteString -> CryptoFailable ByteString
hash salt str = Argon2.hash hashConfig str salt hashLength

-- Hashing parameters
newSalt :: IO ByteString
newSalt = getRandomBytes 64

hashLength :: Int
hashLength = 32

hashConfig :: Argon2.Options
hashConfig =
  Argon2.Options
    { iterations = 2
    , memory = 2 ^ (17 :: Int)
    , parallelism = 4
    , variant = Argon2id
    , version = Version13
    }
