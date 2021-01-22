-- | Create fancy ASCII headings from a string.

module TextUtils.Headings where

import Data.Char
import Data.List (transpose, intercalate)
import Data.Maybe (maybe)

import Data.List.Split (splitOn)
import qualified Data.Map as Map


-- Should also have functions for creating cooler fonts than just monospaced ones.


-- | Map a character to a bunch of lines.
--
-- It's important that each char has equal chars per row.
--
-- You must have the letter 'a' defined, at least. It is used for
-- determining the width and height of the rest of the characters.
type AsciiFont = Map.Map Char [String]


-- FIXME: is it bad that it doesnt autopad? also in the header you should be able to define the definign width/height character
-- if do autopad but maybe that's all bad idea
-- FIXME: add a header to the file that's just a comma-delimited list of settings like uppercase or lowercase.
-- this will require makign AsciiFont a record.
-- FIXME: do consistency checking OR padd them out! choose one!
-- | Create an AsciiFont from a ".bmf` file (burrow monospaced font)...
parseFont :: FilePath -> IO AsciiFont
parseFont path = do
  fontFileContents <- readFile path
  pure $ Map.fromList $ map charLegendsToPair (splitIntoCharLegends fontFileContents)
 where
  splitIntoCharLegends = splitOn "\n\n"
  charLegendsToPair charLegend =
    case lines charLegend of
      x:[] -> error "Char legend misformatted."
      x:xs ->
        case x of
          c:[] -> (c, xs)
          _ -> error "Legend in char legend is not just a single character."
      _ -> error "Char legend misformatted."


-- TODO: In the future a config/ini file will specify which fonts get mapped to what. In this
-- same cnofig other settings will be named.
getAsciiFonts :: IO (Map.Map String AsciiFont)
getAsciiFonts = do
  h1 <- parseFont "data/fonts/basicthorpe.bmf"
  pure $ Map.fromList [("h1", h1)]


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


-- this is hacks in case... update when ini later
fontLookup :: Char -> AsciiFont -> Maybe [String]
fontLookup c f =
  case Map.lookup (toLower c) f of
    Nothing ->
      case Map.lookup (toUpper c) f of
        Nothing -> Nothing
        x -> x
    x -> x


fontWidthHeight :: AsciiFont -> (Int, Int)
fontWidthHeight font =
  -- temporary hack FIXME (see notes about ini)
  case Map.lookup 'a' font of
    Nothing ->
      case Map.lookup 'A' font of
        Nothing -> error "Font width/height error: neither 'a' nor 'A' defined."
        Just a -> determineWidthHeightFromA a
    Just a -> determineWidthHeightFromA a
 where
  determineWidthHeightFromA a =
    let
      firstLineLength = length (head a)
    in
      if firstLineLength < 1
        then error "First line length <0"
        else (length $ head a, length a)


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
