{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Parser (
    replace
  , transformWith
  , titleize
  , chompTrailing
  ) where

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec ()

import Data.List (intercalate)

import qualified Data.Char as C

import qualified Text.Inflections as I
import Text.Inflections.Parse.Types (Word(..))

import Control.Applicative ((<|>), (<*), (*>), (<*>))

import Text.CorasickPark.Types (BoundaryType(..), Target(..))

data MatchSegment = Match String String | Remaining String deriving (Show, Eq)

replace :: String -- ^ Input text string
        -> Target -- ^ Target to match
        -> String -- ^ The string to replace
        -> String -- ^ The Text with substitutions applied
replace input target replacement =
  case parse (parser target) "(input)" input of
    Left _        -> input
    Right matches -> intercalate replacement matches

transformWith :: String -- ^ Input text string
              -> Target -- ^ Target to match
              -> (String -> String)
              -> String -- ^ String with transformation function applied
transformWith input target fn =
    case parse (parser target) "(input)" input of
    Left _        -> input
    Right matches -> intercalate withFn matches

    where withFn = fn (text target)

titleize :: String -- ^ Input text string
         -> Target -- ^ Target to match
         -> String -- ^ String with matching terms titleized
titleize input target =
    case parse (parser target) "(input)" input of
    Left _        -> input
    Right matches -> intercalate titleized matches

    where titleized = I.titleize $ map Word $ words (text target)

chompTrailing :: String -- ^ Input text string
              -> Target -- ^ Target to match
              -> String -- ^ String with text trailing matches removed
chompTrailing input target =
    case parse (parser target) "(input)" input of
    Left _        -> input
    Right matches ->
      intercalate (text target) $ (map snd $ filter (odd . fst) $
      zip [(1 :: Integer)..] matches) ++ [""]

parser :: Target -> Parsec String () [String]
parser target = do
  sections <- if (global target) then
                many (try (targetParser target))

              else
                do
                  ss <- try (targetParser target)
                  return [ss]

  toEnd <- manyTill anyChar eof

  return $ sections ++ [toEnd]

caseInsensitiveChar c = char (C.toLower c) <|> char (C.toUpper c)

caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

lhsParser :: Parsec String () String
          -> BoundaryType
          -> Parsec String () (String, String)
lhsParser strParser NoBoundary = do
  prefix <- manyTill anyChar (lookAhead (try strParser))
  match <- strParser
  return (prefix, match)

lhsParser strParser InputBoundary = do
  match <- strParser
  return ("", match)

lhsParser strParser LineBoundary =
  (try (lhsParser strParser InputBoundary)) <|> lineBoundaryParser

  where lineBoundaryParser = do
          toLineBoundary <-
            manyTill anyChar (lookAhead (try (char '\n' <|> char '\r') *> strParser))

          newlineChar <- (char '\n' <|> char '\r')
          match <- strParser
          return (toLineBoundary ++ [newlineChar], match)

lhsParser strParser WordBoundary =
  try (lhsParser strParser LineBoundary)
  <|> wordBoundaryParser

  where wordBoundaryParser = do
          toMatchBoundary <-
            manyTill anyChar (lookAhead (try (satisfy (not . C.isAlpha) *> strParser)))

          nonWordChar <- satisfy (not . C.isAlpha)
          match <- strParser
          return (toMatchBoundary ++ [nonWordChar], match)

rhsParser :: BoundaryType -> Parsec String () String
rhsParser NoBoundary = manyTill anyChar eof
rhsParser InputBoundary = eof >> return ""

rhsParser LineBoundary =
  (try (rhsParser InputBoundary)) <|> do
    eolChar <- (char '\n' <|> char '\r')
    rest <- manyTill anyChar eof
    return $ [eolChar] ++ rest

rhsParser WordBoundary =
  try (rhsParser LineBoundary)
  <|> do
    notWordChar <- satisfy (not . C.isAlpha)
    rest <- manyTill anyChar eof
    return $ [notWordChar] ++ rest

targetParser :: Target -> Parsec String () String
targetParser Target { text           = txt
                    , caseSensitive  = isCaseSensitive
                    , leftBoundary   = lboundary
                    , rightBoundary  = rboundary
                    } = do

  (prefix, _) <- lhsParser casedText lboundary
  _ <- lookAhead (try (rhsParser rboundary))

  return prefix

  where casedText = if isCaseSensitive
                      then string txt
                      else caseInsensitiveString txt
