{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.AlgorithmSpec (spec) where

import Test.Hspec

import Text.CorasickPark.Types
import Text.CorasickPark.Algorithm

spec :: Spec
spec = do
  describe "downcase" $ do
    it "changes the target to lower-case" $ do
      let op = Operation { target = Target { text          = "FOO"
                                           , caseSensitive = True
                                           , leftBoundary  = InputBoundary
                                           , rightBoundary = InputBoundary
                                           , global        = False }
                         , transform = Downcase }

      applyOperation op "FOO" `shouldBe` "foo"

  describe "upcase" $ do
    it "changes the target to upper-case" $ do
      let op = Operation { target = Target { text          = "foo"
                                           , caseSensitive = True
                                           , leftBoundary  = InputBoundary
                                           , rightBoundary = InputBoundary
                                           , global        = False }
                         , transform = Upcase }

      applyOperation op "foo" `shouldBe` "FOO"

  describe "replace" $ do
    describe "with NoBoundary on RHS and LHS" $ do
      it "matches when the string matches" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Replace "bar" }


        applyOperation op "beforefooafter" `shouldBe` "beforebarafter"

      it "does not replace when pattern does not match" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op  "beforeNOTHINGafter" `shouldBe` "beforeNOTHINGafter"

      it "matches strings, case insensitive" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "BEforeFOOafter" `shouldBe` "BEforebarafter"

      it "doesn't modify the case of the original string if boundaries don't match" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = InputBoundary
                                             , rightBoundary = InputBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "before fOo after" `shouldBe` "before fOo after"

    describe "with WordBoundary" $ do
      it "matches when the word boundary is on both sides of the word" $ do
        let op = Operation { target = Target  { text          = "foo"
                                              , caseSensitive = False
                                              , leftBoundary  = WordBoundary
                                              , rightBoundary = WordBoundary
                                              , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "before foo after" `shouldBe` "before bar after"

      it "does not match when there isn't a word boundary" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = WordBoundary
                                             , rightBoundary = WordBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "beforefooafter" `shouldBe` "beforefooafter"

    describe "with LineBoundary" $ do
      it "matches strings" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = LineBoundary
                                             , rightBoundary = LineBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "before\nfoo\nafter" `shouldBe` "before\nbar\nafter"

      it "does not match when there isn't a line boundary before and after the terms" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = LineBoundary
                                             , rightBoundary = LineBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "beforefooafter" `shouldBe` "beforefooafter"

    describe "with InputBoundary" $ do
      it "matches strings" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = InputBoundary
                                             , rightBoundary = InputBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "foo" `shouldBe` "bar"

      it "does not match when input boundaries don't match" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = InputBoundary
                                             , rightBoundary = InputBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "before\nfoo\nafter" `shouldBe` "before\nfoo\nafter"

    describe "global replacement" $ do
      it "replaces all matches when True" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = True }
                           , transform = Replace "bar" }

        applyOperation op "foo foo foo" `shouldBe` "bar bar bar"

      it "replaces the first match when False" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Replace "bar" }

        applyOperation op "foo foo foo" `shouldBe` "bar foo foo"

    describe "unicode replacement" $ do
      it "replaces a unicode target with ASCII" $ do
        let op = Operation { target = Target { text          = "你好世界"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Replace "Hello World" }

        applyOperation op "Hello World" `shouldBe` "Hello World"

      it "replaces ASCII with unicode" $ do
        let op = Operation { target = Target { text          = "Hello World"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Replace "你好世界" }
        applyOperation op "Hello World" `shouldBe` "你好世界"

    describe "truncate trailing" $ do
      it "removes text trailing the match" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = TruncateTrailing }

        applyOperation op "foo bar" `shouldBe` "foo"

      it "does not change capitalization of the match" $ do
        let op = Operation { target = Target { text          = "fOo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = TruncateTrailing }

        applyOperation op "foo bar" `shouldBe` "foo"

      it "removes text trailing multiple matches when target is global" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = True }
                           , transform = TruncateTrailing }

        applyOperation op "some foo bar foo baz foo buzz"
          `shouldBe` "some foofoofoo"

      it "removes all trailing text when multiple matches are found but the target is not global" $ do
        let op = Operation { target = Target { text          = "foo"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = TruncateTrailing }

        applyOperation op "some foo bar foo baz foo buzz" `shouldBe` "some foo"

    describe "titleize" $ do
      it "titleizes the string" $ do
        let op = Operation { target = Target { text          = "green tree"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = False }
                           , transform = Titleize }

        applyOperation op "green tree" `shouldBe` "Green Tree"

      it "titleizes many occurrences of the string when global is enabled" $ do
        let op = Operation { target = Target { text          = "green tree"
                                             , caseSensitive = False
                                             , leftBoundary  = NoBoundary
                                             , rightBoundary = NoBoundary
                                             , global        = True }
                           , transform = Titleize }

        applyOperation op "there is a green tree the green tree is very green"
          `shouldBe` "there is a Green Tree the Green Tree is very green"
