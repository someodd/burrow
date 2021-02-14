-- TODO: variableWidth flag, but must still be consistent height.

-- | Create fancy ASCII-art-type text from an input string.
--
-- Burrow has support for its own font specification, which is used
-- just for creating ASCII-art headings. See the README for more
-- details on the font specification and configuring Burrow to use
-- different fonts.

module TextUtils.Headings
  ( HeadingLevelFontMap
  , AsciiFont
  , headingCompose
  , getAsciiFonts
  )
where

import Data.Char
import Data.List (transpose, intercalate)

import Data.List.Split (splitOn)
import qualified Data.Map as Map

import Config


-- | An ASCII art character is represented by breaking up each line into an element of a list.
type AsciiChar = [String]


-- | An ASCII font is represented this way to make it easy to lookup a regular character and
-- get the AsciiChar in return.
type AsciiFont = Map.Map Char AsciiChar


-- | Parse a Burrow ASCII art font file.
--
-- See the project's README for the font spec.
parseFont :: FilePath -> IO AsciiFont
parseFont path = do
  fontFileContents <- readFile path
  pure $ Map.fromList $ map charLegendsToPair (splitIntoCharLegends fontFileContents)
 where
  splitIntoCharLegends = splitOn "\n\n"
  charLegendsToPair charLegend =
    case lines charLegend of
      _:[] -> error "Char legend misformatted."
      x:xs ->
        case x of
          c:[] -> (c, xs)
          _ -> error "Legend in char legend is not just a single character."
      _ -> error "Char legend misformatted."

  -- i can't think of any useful flags for the font!
  -- Parse the meta/header where flags are defined and return the rest.
  --parseMeta


-- | Specifies which AsciiFont is related to which heading level.
type HeadingLevelFontMap = Map.Map Int AsciiFont


-- | Get all the fonts loaded, mapped to a specific heading level,
-- according to the configuration file.
getAsciiFonts :: IO HeadingLevelFontMap
getAsciiFonts = do
  configParser <- getConfig
  let func level = getConfigValue configParser "fonts" ("h" ++ (show level)) >>= parseFont >>= pure . (,) level
  result <- traverse func [1..6]
  pure $ Map.fromList result


-- SHOULD DOCUMENT THE BEHAVIOR OF LOOKING UP. if failure to look up the requested case
-- then get the opposite.
-- | Look up the ascii art character which corresponds to the supplied character.
--
-- Hacky and lazy. Will update when specify flags later (like allUpper, allLower) in the spec.
fontLookup :: Char -> AsciiFont -> Maybe [String]
fontLookup c f =
  case Map.lookup c f of
    Nothing -> dothing
    x -> x
 where
  dothing
    | isAsciiLower c = Map.lookup (toUpper c) f
    | isAsciiUpper c = Map.lookup (toLower c) f
    | otherwise = Nothing


-- | The font must be of a consistent height. We sample the first character
-- we come across.
getCharacterHeight :: AsciiFont -> Int
getCharacterHeight font =
  -- FIXME/TODO: just use a error exception handler
  if Map.null font
    then error "While trying to get height: no characters defined."
    else length $ head $ Map.elems font


-- | The main function: ...
headingCompose :: AsciiFont -> String -> String
headingCompose font string = "\n" ++ composeJoin
 where
  characterHeight :: Int
  characterHeight = getCharacterHeight font

  fontLetter :: Char -> [String]
  fontLetter c =
    case fontLookup c font of
      Just a -> a
      Nothing -> replicate characterHeight ""

  convert = map fontLetter string

  composeJoin = unlines $ map (intercalate "") $ transpose convert
