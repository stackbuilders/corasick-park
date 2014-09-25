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
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        replace "beforefooafter" tgt "bar" `shouldBe` "beforebarafter"

      it "does not replace when pattern does not match" $ do
        let tgt = Target { text          = "foo"
                      , caseSensitive = False
                      , leftBoundary  = NoBoundary
                      , rightBoundary = NoBoundary
                      , global        = False }

        replace "beforeNOTHINGafter" tgt "bar" `shouldBe` "beforeNOTHINGafter"

      it "matches strings, case insensitive" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        replace "BEforeFOOafter" tgt "bar" `shouldBe` "BEforebarafter"

      it "doesn't modify the case of the original string if boundaries don't match" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = InputBoundary
                         , rightBoundary = InputBoundary
                         , global        = False }

        replace "before fOo after" tgt "bar" `shouldBe` "before fOo after"

    describe "with WordBoundary" $ do
      it "matches when the word boundary is on both sides of the word" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = WordBoundary
                         , rightBoundary = WordBoundary
                         , global        = False }

        replace "before foo after" tgt "bar" `shouldBe` "before bar after"

      it "does not match when there isn't a word boundary" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = WordBoundary
                         , rightBoundary = WordBoundary
                         , global        = False }

        replace "beforefooafter" tgt "bar" `shouldBe` "beforefooafter"

    describe "with LineBoundary" $ do
      it "matches strings" $ do
        let tgt = Target { text          = "foo"
                            , caseSensitive = False
                            , leftBoundary  = LineBoundary
                            , rightBoundary = LineBoundary
                            , global        = False }

        replace "before\nfoo\nafter" tgt "bar" `shouldBe` "before\nbar\nafter"

      it "does not match when there isn't a line boundary before and after the terms" $ do
        let tgt = Target { text          = "foo"
                            , caseSensitive = False
                            , leftBoundary  = LineBoundary
                            , rightBoundary = LineBoundary
                            , global        = False }

        replace "beforefooafter" tgt "bar" `shouldBe` "beforefooafter"

    describe "with InputBoundary" $ do
      it "matches strings" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = InputBoundary
                         , rightBoundary = InputBoundary
                         , global        = False }

        replace "foo" tgt "bar" `shouldBe` "bar"

      it "does not match when input boundaries don't match" $ do
        let tgt = Target { text          = "foo"
                            , caseSensitive = False
                            , leftBoundary  = InputBoundary
                            , rightBoundary = InputBoundary
                            , global        = False }

        replace "before\nfoo\nafter" tgt "bar" `shouldBe` "before\nfoo\nafter"

    describe "global replacement" $ do
      it "replaces all matches when True" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = True }

        replace "foo foo foo" tgt "bar" `shouldBe` "bar bar bar"

      it "replaces the first match when False" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        replace "foo foo foo" tgt "bar" `shouldBe` "bar foo foo"

    describe "unicode replacement" $ do
      it "replaces a unicode target with ASCII" $ do
        let tgt = Target { text          = "你好世界"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        replace "Hello World" tgt "Hello World" `shouldBe` "Hello World"

      it "replaces ASCII with unicode" $ do
        let tgt = Target { text          = "你好世界"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        replace "你好世界" tgt "Hello World" `shouldBe` "Hello World"

    describe "chompTrailing" $ do
      it "removes text trailing the match" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        chompTrailing "foo bar" tgt `shouldBe` "foo"

      it "does not change capitalization of the match" $ do
        let tgt = Target { text          = "fOo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        chompTrailing "foo bar" tgt `shouldBe` "foo"

      it "removes text trailing multiple matches when target is global" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = True }

        chompTrailing "some foo bar foo baz foo buzz" tgt `shouldBe` "some foofoofoo"

      it "removes all trailing text when multiple matches are found but the target is not global" $ do
        let tgt = Target { text          = "foo"
                         , caseSensitive = False
                         , leftBoundary  = NoBoundary
                         , rightBoundary = NoBoundary
                         , global        = False }

        chompTrailing "some foo bar foo baz foo buzz" tgt `shouldBe` "some foo"
