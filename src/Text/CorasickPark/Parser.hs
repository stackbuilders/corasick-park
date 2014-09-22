{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Parser (replace) where

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec ()

import qualified Data.Char as C

import Control.Applicative ((<|>))

import Text.CorasickPark.Types (BoundaryType(..))

-- | Text before and after the match is found.
type TextEnds = (String, String)

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
    Right matches -> insertReplacements target replacement boundaries matches

  where parsed =
          parse (parser isCaseSensitive isGlobal target) "(input)" input


-- | Returns a function that retrieves the nearest charactero to a match,
-- for examining the boundary of the match.
nearestCharFn :: Side -> String -> Char
nearestCharFn RightSide = head
nearestCharFn LeftSide  = last


spliceText :: String -- ^ Pattern matched
           -> String -- ^ Text to substitute
           -> (BoundaryType, BoundaryType) -- ^ Boundaries we need to verify
           -> TextEnds -- ^ Text chunks broken by matches of pattern
           -> String

spliceText pattern replacement (lboundary, rboundary) (lside, rside) =
  if sidesAreValid then
    lside ++ replacement

  else
    lside ++ pattern

  where sidesAreValid =
          validBoundary lboundary LeftSide lside &&
          validBoundary rboundary RightSide rside

parser :: Bool -> Bool -> String -> Parsec String () [String]
parser isCaseSensitive isGlobal target = do
  sections <- if isGlobal then
                many (try (matchingSection isCaseSensitive target))

              else
                do
                  ss <- try (matchingSection isCaseSensitive target)
                  return [ss]

  toEnd <- manyTill anyChar eof

  return $ sections ++ [toEnd]

insertReplacements :: String -- ^ Pattern matched
                  -> String -- ^ Text to substitute
                  -> (BoundaryType, BoundaryType) -- ^ Boundaries to verify
                  -> [String] -- ^ Text chunks broken by matches of pattern
                  -> String
insertReplacements _ _ _ [] = ""
insertReplacements _ _ _ (x:[]) = x
insertReplacements pattern replacement (lboundary, rboundary) (x:y:zs) =
  splicedText ++
  insertReplacements pattern replacement (lboundary, rboundary) (y : zs)

  where splicedText =
          spliceText pattern replacement (lboundary, rboundary) (x, y)


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


matchingSection :: Bool -> String -> Parsec String () String
matchingSection isCaseSensitive target =
  manyTill anyChar (try (caseString isCaseSensitive target))


caseString :: Bool -> String -> Parsec String () String
caseString True target = string target
caseString False target =
  do { walk target; return target }

  where
    walk []     = return ()
    walk (c:cs) = do { _ <- caseChar c <?> msg; walk cs }

    caseChar c  | C.isAlpha c  = char (C.toLower c) <|> char (C.toUpper c)
                | otherwise  = char c

    msg         = show target
