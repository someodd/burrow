-- | Create fancy ASCII headings from a string.

module TextUtils.Headings where

import Data.Char
import Data.List (transpose, intercalate)
import Data.Maybe (maybe)

import qualified Data.Map as Map


-- Should also have functions for creating cooler fonts than just monospaced ones.


-- | Map a character to a bunch of lines.
--
-- It's important that each char has equal chars per row.
--
-- You must have the letter 'a' defined, at least. It is used for
-- determining the width and height of the rest of the characters.
type AsciiFont = Map.Map Char [String]


-- Need to make it so you can actually load in fonts from files
h1 :: AsciiFont
h1 =
  let
    a =
      [ " # "
      , "# #"
      , "###"
      , "# #"
      , "# #"
      ]
    b =
      [ "## "
      , "# #"
      , "###"
      , "# #"
      , "###"
      ]
    c =
      [ "###"
      , "#  "
      , "#  "
      , "#  "
      , "###"
      ]
  in
    Map.fromList [('a', a), ('b', b), ('c', c)]


fontLookup :: Char -> AsciiFont -> Maybe [String]
fontLookup c = Map.lookup (toLower c)


fontWidthHeight :: AsciiFont -> (Int, Int)
fontWidthHeight font =
  case Map.lookup 'a' font of
    Just [] -> error "Font letter 'a' is blank: []!"
    Just a ->
      let
        firstLineLength = length (head a)
      in
        if firstLineLength < 1
          then error "First line length <0"
          else (length $ head a, length a)
    Nothing -> error "Font does not have 'a'!"


headingCompose :: AsciiFont -> String -> String
headingCompose font string = "\n" ++ composeJoin
 where
  (_, height) = fontWidthHeight font

  fontLetter font c =
    case fontLookup c font of
      Just a -> a
      Nothing -> replicate height ""

  convert = map (fontLetter font) string

  addSpace = map (fmap (++ " "))

  composeJoin = unlines $ map (intercalate "") $ transpose (addSpace convert)
