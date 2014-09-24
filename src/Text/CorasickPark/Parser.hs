{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Parser (replace) where

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec ()

import qualified Data.Char as C

import Control.Applicative ((<|>))

import Text.CorasickPark.Types (BoundaryType(..))

data Side = LeftSide | RightSide


replace :: String
        -- ^ Input text string

        -> String
        -- ^ Target string

        -> String
        -- ^ Replacement

        -> (BoundaryType, BoundaryType)
        -- ^ (Left Boundary, Right Boundary) for matched pattern

        -> Bool
        -- ^ Whether the matched pattern is case-sensitive

        -> Bool
        -- ^ Whether to replace all occurrences

        -> String
        -- ^ The Text with substitutions applied

replace input target replacement boundaries isCaseSensitive isGlobal =
  case parsed of
    Left _        -> input
    Right matches -> insertReplacements replacement boundaries matches

  where parsed =
          parse (parser isCaseSensitive isGlobal target) "(input)" input


-- | Returns a function that retrieves the nearest charactero to a match,
-- for examining the boundary of the match.
nearestCharFn :: Side -> String -> Char
nearestCharFn RightSide = head
nearestCharFn LeftSide  = last

validBoundaries :: (BoundaryType, BoundaryType) -> (String, String) -> Bool
validBoundaries (lboundary, rboundary) (lside, rside) =
  validBoundary lboundary LeftSide lside &&
  validBoundary rboundary RightSide rside


spliceText :: String -- ^ Replacement text
           -> (BoundaryType, BoundaryType) -- ^ Boundaries we need to verify
           -> (MatchSegment, MatchSegment) -- ^ Text chunks broken by matches of pattern
           -> String

spliceText replacement bounds (Match lprefix lmatched, Match rprefix rmatched) =
  if validBoundaries bounds (lprefix, rprefix) then
    lprefix ++ replacement

  else
    lprefix ++ lmatched

spliceText replacement bounds (Match lprefix lmatched, Remaining str) =
  if validBoundaries bounds (lprefix, str) then
    lprefix ++ replacement

  else
    lprefix ++ lmatched

spliceText replacement bounds (Remaining str1, Remaining str2) =
  str1 ++ str2

spliceText replacement bounds (Remaining str1, Match prefix matched) =
  str1 ++ prefix


data MatchSegment = Match String String | Remaining String deriving (Show, Eq)

parser :: Bool -> Bool -> String -> Parsec String () [MatchSegment]
parser isCaseSensitive isGlobal target = do
  sections <- if isGlobal then
                many (try (matchingSection isCaseSensitive target))

              else
                do
                  ss <- try (matchingSection isCaseSensitive target)
                  return [ss]

  toEnd <- manyTill anyChar eof

  return $ sections ++ [Remaining toEnd]

insertReplacements :: String -- ^ Text to substitute
                  -> (BoundaryType, BoundaryType) -- ^ Boundaries to verify
                  -> [MatchSegment] -- ^ Text chunks broken by matches of pattern
                  -> String
insertReplacements _ _ [] = ""
insertReplacements _ _ (Match prefix matchedText:[]) = prefix ++ matchedText
insertReplacements _ _ (Remaining str:[]) = str
insertReplacements replacement (lboundary, rboundary) (x:y:zs) =
  splicedText ++
  insertReplacements replacement (lboundary, rboundary) (y : zs)

  where splicedText =
          spliceText replacement (lboundary, rboundary) (x, y)


-- | Figures out if the given string is a valid boundary for each match type.
-- Note that we take care of null cases by looking up to see if it matches an
-- input boundary, which is basically just a null check on the matched text.
validBoundary :: BoundaryType -> Side -> String -> Bool

validBoundary NoBoundary _ _ = True

validBoundary InputBoundary _ t = null t

validBoundary LineBoundary s t =
  validBoundary InputBoundary s t || (nearestC == '\n') || (nearestC == '\r')
  where nearestC = nearestCharFn s t

validBoundary WordBoundary s t =
  validBoundary LineBoundary s t
  || (not . C.isAlpha) (nearestCharFn s t)


matchingSection :: Bool -> String -> Parsec String () MatchSegment
matchingSection isCaseSensitive target = do
  prefix <- manyTill anyChar (lookAhead (try casedText))
  matchedTarget <- casedText
  return $ Match prefix matchedTarget

  where casedText = if isCaseSensitive
                      then string target
                      else caseInsensitiveString target

caseInsensitiveChar c = char (C.toLower c) <|> char (C.toUpper c)
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""
