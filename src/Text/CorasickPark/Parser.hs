{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Parser (replace) where

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec ()

import Data.List (intercalate)

import qualified Data.Char as C

import Control.Applicative ((<|>), (<*))

import Text.CorasickPark.Types (BoundaryType(..), Target(..))

data MatchSegment = Match String String | Remaining String deriving (Show, Eq)

replace :: String
        -- ^ Input text string

        -> Target -- ^ Describes what to match

        -> String -- ^ The string to replace
        -> String
        -- ^ The Text with substitutions applied

replace input target replacement =
  case parse (parser target) "(input)" input of
    Left _        -> input
    Right matches -> intercalate replacement matches


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

endingInWordBoundary :: Parsec String () String
endingInWordBoundary = do
  tillEnd <- manyTill anyChar (lookAhead (try (satisfy (not . C.isAlpha))))
  lastChar <- satisfy (not . C.isAlpha)
  eof
  return $ tillEnd ++ [lastChar]

lhsParser :: BoundaryType -> Parsec String () String
lhsParser NoBoundary = manyTill anyChar eof
lhsParser InputBoundary = eof >> return ""

lhsParser LineBoundary =
  (try (lhsParser InputBoundary) <|> manyTill anyChar (char '\n' <|> char '\r'))
  <* eof

lhsParser WordBoundary =
  (try (lhsParser InputBoundary)) <|> (try (lhsParser LineBoundary))
  <|> endingInWordBoundary

rhsParser :: BoundaryType -> Parsec String () String
rhsParser NoBoundary = manyTill anyChar eof
rhsParser InputBoundary = eof >> return ""

rhsParser LineBoundary =
  (try (rhsParser InputBoundary)) <|> do
    eolChar <- (char '\n' <|> char '\r')
    rest <- manyTill anyChar eof
    return $ [eolChar] ++ rest

rhsParser WordBoundary =
  (try (rhsParser LineBoundary)) <|> do
    notWordChar <- satisfy (not . C.isAlpha)
    rest <- manyTill anyChar eof
    return $ [notWordChar] ++ rest

targetParser :: Target -> Parsec String () String
targetParser Target { text = txt
                    , caseSensitive = isCaseSensitive
                    , leftBoundary = lboundary
                    , rightBoundary = rboundary
                    } = do

  prefix <- manyTill anyChar (lookAhead (try casedText))
  _  <- casedText
  suffix <- lookAhead (try (manyTill anyChar eof))

  case parse (lhsParser lboundary) "(prefix)" prefix of
    Left _ -> fail "Invalid LHS boundary"
    Right _ ->
      case parse (rhsParser rboundary) "(suffix)" suffix of
        Left _ -> fail "Invalid RHS boundary"
        Right _ -> return prefix

  where casedText = if isCaseSensitive
                      then string txt
                      else caseInsensitiveString txt
