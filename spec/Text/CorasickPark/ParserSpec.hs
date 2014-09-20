{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.ParserSpec (spec) where

import Test.Hspec

import Text.CorasickPark.Types
import Text.CorasickPark.Parser

spec :: Spec
spec = do
  describe "replace" $ do
    describe "with NoBoundary on RHS and LHS" $ do
      it "matches when the string matches" $ do
        replace "beforefooafter" "foo" "bar"
          (NoBoundary, NoBoundary) True False `shouldBe` "beforebarafter"

      it "does not replace when pattern does not match" $ do
        replace "beforeNOTHINGafter" "foo" "bar"
          (NoBoundary, NoBoundary) True False `shouldBe` "beforeNOTHINGafter"

      it "matches strings, case insensitive" $ do
        replace "BEforeFOOafter" "foo" "bar"
          (NoBoundary, NoBoundary) False False `shouldBe` "BEforebarafter"

    describe "with WordBoundary" $ do
      it "matches when the word boundary is on both sides of the word" $ do
        replace "before foo after" "foo" "bar"
          (WordBoundary, WordBoundary) True False `shouldBe` "before bar after"

      it "does not match when there isn't a word boundary" $ do
        replace "beforefooafter" "foo" "bar"
          (WordBoundary, WordBoundary) True False `shouldBe` "beforefooafter"

    describe "with LineBoundary" $ do
      it "matches strings" $ do
        replace "before\nfoo\nafter" "foo" "bar"
          (LineBoundary, LineBoundary) True False `shouldBe` "before\nbar\nafter"

      it "does not match when there isn't a line boundary before and after the terms" $ do
        replace "beforefooafter" "foo" "bar"
          (LineBoundary, LineBoundary) True False `shouldBe` "beforefooafter"

    describe "with InputBoundary" $ do
      it "matches strings" $ do
        replace "foo" "foo" "bar"
          (InputBoundary, InputBoundary) True False `shouldBe` "bar"

      it "does not match when there isn't a line boundary before and after the terms" $ do
        replace "before\nfoo\nafter" "foo" "bar"
          (InputBoundary, InputBoundary) True False `shouldBe` "before\nfoo\nafter"

    describe "global replacement" $ do
      it "replaces all matches when True" $ do
        replace "foo foo foo" "foo" "bar"
          (NoBoundary, NoBoundary) True True `shouldBe` "bar bar bar"


      it "replaces the first match when False" $ do
        replace "foo foo foo" "foo" "bar"
          (NoBoundary, NoBoundary) True False `shouldBe` "bar foo foo"
