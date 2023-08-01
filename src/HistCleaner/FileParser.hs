{-# LANGUAGE OverloadedStrings #-}

module HistCleaner.FileParser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List

data FileType
  = Generic -- TODO: shell history with access time?
  | Lesshst
  | Viminfo
  deriving (Eq, Show)

data ParsedLine
  = Skip
  | Simple [ByteString]
  | Special ByteString [ByteString]

getFileType :: FilePath -> FileType
getFileType filepath
  | "lesshst" `isSuffixOf` filepath = Lesshst
  | "viminfo" `isSuffixOf` filepath = Viminfo
  | otherwise = Generic

parseLine :: FileType -> ByteString -> ParsedLine
parseLine fileType line =
  case fileType of
    Generic -> Simple $ C8.words line
    Lesshst -> parseLesshst line
    _ -> error $ show fileType <> "currently unsupported"

parseLesshst :: ByteString -> ParsedLine
parseLesshst line --TODOODOAO: Wie skippen wir für die Punkt-linien?
 =
  if C8.isPrefixOf "." line
    then Skip
    else Simple $ C8.words $ C8.dropWhile (== '\"') line

reFormatLine :: FileType -> [ByteString] -> ByteString
reFormatLine fileType line =
  case fileType of
    Generic -> C8.unwords line
    Lesshst -> reFormatLesshst line
    _ -> error $ show fileType <> "currently unsupported"

reFormatLesshst :: [ByteString] -> ByteString
reFormatLesshst line = C8.unwords (C8.pack ['"'] : line)
