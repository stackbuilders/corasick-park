{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Parser
       ( parseToStrings
       , parseToMatchSegments
       ) where

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec ()

import qualified Data.Char as C


import Control.Applicative ((<|>), (*>))

import Text.CorasickPark.Types ( BoundaryType(..)
                               , Target(..)
                               , MatchSegment(..) )

parseToStrings :: String -> Target -> [String]
parseToStrings string target =
  case parse (parser target) "(input)" string of
    Left _ -> [string]
    Right matches -> matches

parseToMatchSegments :: String -> Target -> [MatchSegment]
parseToMatchSegments string target =
  case parse (parserWithSegments target) "(input)" string of
    Left _ -> [Remaining string]
    Right matches -> matches

parser :: Target -> Parsec String () [String]
parser target = do
  segments <- parserWithSegments target
  return $ map nonMatchString segments

parserWithSegments :: Target -> Parsec String () [MatchSegment]
parserWithSegments target = do
  sections <- if global target then
                many (try (targetParser target))

              else
                do
                  ss <- try (targetParser target)
                  return [ss]

  toEnd <- manyTill anyChar eof

  return $ sections ++ [Remaining toEnd]

nonMatchString :: MatchSegment -> String
nonMatchString (Match pre _)   = pre
nonMatchString (Remaining str) = str

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
  try (lhsParser strParser InputBoundary) <|> lineBoundaryParser

  where lineBoundaryParser = do
          toLineBoundary <-
            manyTill anyChar (lookAhead (try (char '\n' <|> char '\r') *>
                                         strParser))

          newlineChar <- char '\n' <|> char '\r'
          match <- strParser
          return (toLineBoundary ++ [newlineChar], match)

lhsParser strParser WordBoundary =
  try (lhsParser strParser LineBoundary)
  <|> wordBoundaryParser

  where wordBoundaryParser = do
          toMatchBoundary <-
            manyTill anyChar (lookAhead (try (satisfy (not . C.isAlpha) *>
                                              strParser)))

          nonWordChar <- satisfy (not . C.isAlpha)
          match <- strParser
          return (toMatchBoundary ++ [nonWordChar], match)

rhsParser :: BoundaryType -> Parsec String () String
rhsParser NoBoundary = manyTill anyChar eof
rhsParser InputBoundary = eof >> return ""

rhsParser LineBoundary =
  try (rhsParser InputBoundary) <|> do
    eolChar <- char '\n' <|> char '\r'
    rest <- manyTill anyChar eof
    return $ eolChar : rest

rhsParser WordBoundary =
  try (rhsParser LineBoundary)
  <|> do
    notWordChar <- satisfy (not . C.isAlpha)
    rest <- manyTill anyChar eof
    return $ notWordChar : rest

targetParser :: Target -> Parsec String () MatchSegment
targetParser Target { text           = txt
                    , caseSensitive  = isCaseSensitive
                    , leftBoundary   = lboundary
                    , rightBoundary  = rboundary
                    } = do

  (prefix, match) <- lhsParser casedText lboundary
  _ <- lookAhead (try (rhsParser rboundary))

  return $ Match prefix match

  where casedText = if isCaseSensitive
                      then string txt
                      else caseInsensitiveString txt

        caseInsensitiveChar c = char (C.toLower c) <|> char (C.toUpper c)

        caseInsensitiveString s =
          try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""
