{-# LANGUAGE OverloadedStrings #-}

module CleanerSpec where

import Control.Monad.State.Lazy
import Data.ByteString (ByteString)
import Test.Hspec

import HistCleaner.Cleaner
import HistCleaner.FileParser

spec :: Spec
spec =
  parallel $ do
    describe "Test of Cleaner algorithms" $ do
      reduceListToUnchecked
      cleanSecretsFromText

reduceListToUnchecked :: Spec
reduceListToUnchecked = do
  describe "reduce text to unchecked lines" $ do
    it "endLines exist" $
      shouldBe
        (dropAlreadyChecked ["1", "2", "3"] ["1", "2", "3", "4", "5"])
        ["4", "5"]
    it "everything already checked" $
      shouldBe (dropAlreadyChecked ["3", "4", "5"] ["1", "2", "3", "4", "5"]) []
    it "endLines empty" $
      shouldBe
        (dropAlreadyChecked [] ["1", "2", "3", "4", "5"])
        ["1", "2", "3", "4", "5"]
    it "text is empty but endLines exist" $
      shouldBe (dropAlreadyChecked ["1", "2", "3"] []) []
    it "text is shorter than endLines" $
      shouldBe (dropAlreadyChecked ["1", "2", "3"] ["1", "2"]) ["1", "2"]
    it "first and 2nd line of endLines appear twice" $
      shouldBe
        (dropAlreadyChecked ["2", "3", "4"] ["1", "2", "3", "9", "2", "3", "4"])
        []
    it "1st and 2nd line of endLines are the last lines of the text" $
      shouldBe
        (dropAlreadyChecked ["3", "4", "5"] ["1", "2", "3", "4"])
        ["1", "2", "3", "4"]
    it "endLines not found but matches for the 1st line exist" $
      shouldBe
        (dropAlreadyChecked ["2", "3", "4"] ["1", "2", "3", "0", "2", "4"])
        ["1", "2", "3", "0", "2", "4"]

cleanSecretsFromText :: Spec
cleanSecretsFromText = do
  describe "Secret test cleaned from input text" $ do
    it "example 1" $
      shouldBe
        (runState (cleanText Generic inLines salt secrets) CSuccess)
        (outLines, CSuccess)

inLines :: [ByteString]
inLines =
  ["Es war einmal", "ein test für ein Programm", "und dieser test schlug fehl."]

outLines :: [ByteString]
outLines =
  [ "Es war einmal"
  , "ein redacted für ein Programm"
  , "und dieser redacted schlug fehl."
  ]

salt :: ByteString
salt =
  "\141\162Z\176z\167\165/\143{\252\150\178\192\GS\145\ESC\218\SUBm\169\SOHS\237g\146>\163Bdk\r\180\&0\248T\131\246\150\255{\212\169\246\219/\226\&5\163Iv\142\205G+\220]\ETB~\tll\159\144"

secrets :: [ByteString]
secrets =
  [ "\166\EOT\241\146d*/\r`\154\230\134v\233\r\140\166\229`\242\200&!\SId\236\193\168\248\214\DEL\224"
  ]
