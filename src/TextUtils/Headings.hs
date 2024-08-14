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
import Data.Maybe (listToMaybe, catMaybes, isJust)
import Data.Foldable (traverse_)


-- | An ASCII art character is represented by breaking up each line into an element of a list.
type AsciiChar = [String]


-- | An ASCII font is represented this way to make it easy to lookup a regular character and
-- get the AsciiChar in return.
type AsciiFont = Map.Map Char AsciiChar


-- | Parse a Burrow ASCII art font file.
--
-- The character width and height is determined from the first entry.
--
-- See the project's README for the font spec.
parseFont :: FilePath -> IO AsciiFont
parseFont path = do
  fontFileContents <- readFile path
  let
    charPairs = map charLegendsToPair (splitIntoCharLegends fontFileContents)
    (width, height) = case charPairs of
      [] -> error "No characters found in font file."
      (_, firstChar):_ ->
        (length (head firstChar), length firstChar)  -- FIXME: use of head here is unsafe
    (validatedCharPairs, warnings) = unzip $ (flip map) charPairs $ \(charTitle, character) ->
      let
        (validatedChar, warnings') = validateCharacter width height charTitle character
        validatedPair = (charTitle, validatedChar)
      in
        (validatedPair, warnings')
  -- display any warnings for this font
  if any isJust warnings
    then do
      putStrLn $ "Warnings encountered while parsing font file: " ++ path ++ "."
      traverse_ putStrLn (catMaybes warnings)
    else
      pure ()
  pure $ Map.fromList validatedCharPairs
 where
  splitIntoCharLegends = splitOn "\n\n"
  charLegendsToPair charLegend =
    case lines charLegend of
      _:[] -> error "Font file is not divided into sections by blank lines."
      x:xs ->
        case x of
          c:[] -> (c, xs)
          _ ->
            error $ "Legend in char legend is not just a single character, ensure if first entry there are no newlines " ++
            "preceding, double check for whitespace, ensure clear double newlines between the entries:\n\n" ++ unlines xs
      _ -> error "Char legend misformatted."

-- | Determine if a character has any errors, attempt to automatically fix them and return
-- any warnings. May possibly raise an error.
validateCharacter
  :: Int
  -- ^ The width the character is supposed to be.
  -> Int
  -- ^ The height the character is supposed to be.
  -> Char
  -- ^ The character title. The key for the pair. Used for errors.
  -> [String]
  -- ^ The to-be AsciiChar to check.
  -> (AsciiChar, Maybe String)
  -- ^ The character after any automatic fixes.
validateCharacter width height characterTitle asciiChar = do
  let
    -- height
    (heightValidated, heightWarning) = case heightCheck asciiChar of
      Nothing -> (asciiChar, Nothing)
      Just a -> (heightFix asciiChar, Just a)
    -- width
    (widthValidated, widthWarning) = case widthCheck heightValidated of
      Nothing -> (heightValidated, Nothing)
      Just a -> (widthFix heightValidated, Just a)
    -- combine warnings
    warnings = catMaybes [heightWarning, widthWarning]
  -- combine warnings and give the final result
  (widthValidated, if null warnings then Nothing else Just $ unlines warnings)
 where
  heightCheck char = if length char /= height then Just ("Expected height (" ++ show height ++ ") is not consistent for \"" ++ [characterTitle] ++ "\"") else Nothing
  heightFix char
    | length char > height = take height char
    | length char < height = take height $ char ++ replicate (height - length char) (replicate width ' ')
    | otherwise = char

  widthCheck char = if any (\x -> length x /= width) char then Just ("Expected width (" ++ show width ++ ") is inconsistent for \"" ++ [characterTitle] ++ "\"") else Nothing
  widthFix char =
    let lineFix line = if length line > width then take width line else line ++ replicate (width - length line) ' '
    in map lineFix char


-- | Specifies which AsciiFont is related to which heading level.
type HeadingLevelFontMap = Map.Map Int AsciiFont


-- FIXME: if defined more than once, will load in the same fonts over and over!
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
getCharacterHeight font
  | Map.null font = error "While trying to get width: no characters defined."
  | otherwise = maybe (error "While trying to get height: character list is empty.") length (listToMaybe $ Map.elems font)


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
