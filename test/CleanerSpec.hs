{-# language OverloadedStrings #-}

module CleanerSpec where

import Test.Hspec
import HistCleaner.Cleaner


spec :: Spec
spec = parallel $ do
  describe "Test of Cleaner algorithms" $ do
    reduceListToUnchecked
    --multiline

reduceListToUnchecked :: Spec
reduceListToUnchecked = do
  describe "reduceListToUnchecked" $ do
    it "endMarker exists" $
      shouldBe
        (dropAlreadyChecked ["1", "2", "3"] ["1", "2", "3", "4", "5"])
        $ Just ["4", "5"]

    it "endMarker empty" $
      shouldBe
        (dropAlreadyChecked [] ["1", "2", "3", "4", "5"])
        $ Just ["1", "2", "3", "4", "5"]

    it "everything already checked" $
      shouldBe
        (dropAlreadyChecked ["3", "4", "5"] ["1", "2", "3", "4", "5"])
        $ Just []

    it "lines is shorter than endMarker" $
      shouldBe
        (dropAlreadyChecked ["1", "2", "3"] [])
        Nothing
  {-
multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe
        (parse example3)
        example3Result

example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph but
list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]

example3Result :: Document
example3Result =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]
-}
