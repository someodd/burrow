{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TextUtils.Containers (loadAllAsciiBoxes, loadAsciiBox, containerize, parseAsciiBox, AsciiBoxConfig, applyContainer) where

import GHC.Generics (Generic)
import Toml (TomlCodec, decodeFileEither, (.=), table, genericCodec, TomlDecodeError)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (listDirectory)
import Data.List (isSuffixOf)
import Control.Monad (forM)

-- Data type for the 'dimensions' section
data DimensionsConfig = DimensionsConfig
  { maxWidth       :: Int
  , minWidth       :: Int
  , paddingTop     :: Int
  , paddingBottom  :: Int
  , paddingLeft    :: Int
  , paddingRight   :: Int
  } deriving (Show, Generic)

-- Automatically derive a TomlCodec for DimensionsConfig
dimensionsCodec :: TomlCodec DimensionsConfig
dimensionsCodec = genericCodec

-- Data type for the 'borders' section
data BordersConfig = BordersConfig
  { top    :: Text
  , bottom :: Text
  , left   :: [Text]
  , right  :: [Text]
  } deriving (Show, Generic)

-- Automatically derive a TomlCodec for BordersConfig
bordersCodec :: TomlCodec BordersConfig
bordersCodec = genericCodec

-- Data type for the 'corner' section
data CornerConfig = CornerConfig
  { cornerTopLeft     :: Text
  , cornerTopRight    :: Text
  , cornerBottomLeft  :: Text
  , cornerBottomRight :: Text
  } deriving (Show, Generic)

-- Automatically derive a TomlCodec for CornerConfig
cornerCodec :: TomlCodec CornerConfig
cornerCodec = genericCodec

-- Data type for the 'text' section
data TextConfig = TextConfig
  { alignment :: Text
  , filler    :: Text
  } deriving (Show, Generic)

-- Automatically derive a TomlCodec for TextConfig
textCodec :: TomlCodec TextConfig
textCodec = genericCodec

-- Main configuration data type that combines all sections
data AsciiBoxConfig = AsciiBoxConfig
  { dimensions :: DimensionsConfig
  , borders    :: BordersConfig
  , corner     :: CornerConfig
  , text       :: TextConfig
  } deriving (Show, Generic)

-- Automatically derive a TomlCodec for AsciiBoxConfig
asciiBoxCodec :: TomlCodec AsciiBoxConfig
asciiBoxCodec =
  AsciiBoxConfig
    <$> table dimensionsCodec "dimensions" .= dimensions
    <*> table bordersCodec "borders" .= borders
    <*> table cornerCodec "corner" .= corner
    <*> table textCodec "text" .= text

-- Function to parse the TOML file
parseAsciiBox :: FilePath -> IO (Either [TomlDecodeError] AsciiBoxConfig)
parseAsciiBox filePath = decodeFileEither asciiBoxCodec filePath

-- Apply the container rules to the body of text
applyContainer :: AsciiBoxConfig -> Text -> Text
applyContainer box body =
    let
        -- Split the body of text into lines
        bodyLines = T.lines body

        -- Calculate the maximum widths of the left and right borders
        maxLeftBorderWidth = maximum (map T.length $ left (borders box))
        maxRightBorderWidth = maximum (map T.length $ right (borders box))

        -- Calculate the longest body line length
        longestBodyLineLength = maximum (map T.length bodyLines)

        -- Calculate the total width based on the longest body line length, minWidth, and maxWidth
        totalWidth = max (minWidth (dimensions box)) (min (maxWidth (dimensions box)) (longestBodyLineLength + paddingLeft (dimensions box) + paddingRight (dimensions box) + maxLeftBorderWidth + maxRightBorderWidth))

        -- Calculate the usable width inside the borders after padding and maximum border width
        usableWidth = totalWidth
                      - paddingLeft (dimensions box) 
                      - paddingRight (dimensions box) 
                      - maxLeftBorderWidth 
                      - maxRightBorderWidth

        -- Add padding to each line of text and align according to the alignment rule
        paddedLines = map (padAndAlign box usableWidth) bodyLines

        -- Apply left and right borders to each line, adding padding as necessary
        borderedLines = zipWith (applySideBorders box maxLeftBorderWidth maxRightBorderWidth usableWidth) [paddingTop (dimensions box)..] paddedLines

        -- Calculate the full width of the lines including borders
        fullLineWidth = maximum (map T.length borderedLines)

        -- Apply the top border
        topBorderLine = constructBorder (top . borders $ box) (cornerTopLeft . corner $ box) (cornerTopRight . corner $ box) fullLineWidth

        -- Apply the bottom border
        bottomBorderLine = constructBorder (bottom $ borders box) (cornerBottomLeft $ corner box) (cornerBottomRight $ corner box) fullLineWidth

        -- Generate the empty padding lines above and below the text
        paddingLine = T.replicate usableWidth (filler $ text box)
        paddedTop = zipWith (applySideBorders box maxLeftBorderWidth maxRightBorderWidth usableWidth) [0..] (replicate (paddingTop $ dimensions box) paddingLine)
        paddedBottomStartIndex = length borderedLines + length paddedTop
        paddedBottom = zipWith (applySideBorders box maxLeftBorderWidth maxRightBorderWidth usableWidth) [paddedBottomStartIndex..] (replicate (paddingBottom $ dimensions box) paddingLine)

    in
        T.unlines $ topBorderLine : (paddedTop ++ borderedLines ++ paddedBottom ++ [bottomBorderLine])

-- Apply the left and right borders to a line of text
applySideBorders :: AsciiBoxConfig -> Int -> Int -> Int -> Int -> Text -> Text
applySideBorders box maxLeftWidth maxRightWidth usableWidth lineIndex text' =
    let leftBorders = left (borders box)
        rightBorders = right (borders box)
        leftBorder = leftBorders !! (lineIndex `mod` length leftBorders)
        rightBorder = rightBorders !! (lineIndex `mod` length rightBorders)
        paddedLeftBorder = leftBorder <> T.replicate (maxLeftWidth - T.length leftBorder) " " 
        paddedRightBorder = T.replicate (maxRightWidth - T.length rightBorder) " " <> rightBorder
        paddedText = T.justifyLeft usableWidth ' ' text'
    in paddedLeftBorder <> T.replicate (paddingLeft $ dimensions box) " " <> paddedText <> T.replicate (paddingRight $ dimensions box) " " <> paddedRightBorder

-- | Repeat a Text and then grab the first n characters
repeatAndTake :: Int -> Text -> Text
repeatAndTake n txt = T.take n (T.replicate ((n `div` T.length txt) + 1) txt)

-- Helper function to construct the top or bottom border
constructBorder :: Text -> Text -> Text -> Int -> Text
constructBorder horizontalBorder leftCorner rightCorner width =
    leftCorner <> (repeatAndTake (width - T.length leftCorner - T.length rightCorner) horizontalBorder) <> rightCorner

-- Helper function to pad and align text according to the alignment setting
padAndAlign :: AsciiBoxConfig -> Int -> Text -> Text
padAndAlign box width text' =
    let alignment' = alignment (text box)
        fillerChar = case T.uncons (filler $ text box) of
                       Just (c, _) -> c
                       Nothing -> ' '
    in case alignment' of
        "left"   -> justifyLeft width fillerChar text'
        "right"  -> justifyRight width fillerChar text'
        "center" -> center width fillerChar text'
        _        -> justifyLeft width fillerChar text'  -- Default to left alignment

-- Define justifyLeft, justifyRight, and center functions
justifyLeft :: Int -> Char -> Text -> Text
justifyLeft n c t =
  let len = T.length t
  in if len >= n
     then t
     else t <> T.replicate (n - len) (T.singleton c)

justifyRight :: Int -> Char -> Text -> Text
justifyRight n c t =
  let len = T.length t
  in if len >= n
     then t
     else T.replicate (n - len) (T.singleton c) <> t

center :: Int -> Char -> Text -> Text
center n c t =
  let len = T.length t
  in if len >= n
     then t
     else
       let totalPadding = n - len
           (leftPadding, rightPadding) = (totalPadding `div` 2, totalPadding - totalPadding `div` 2)
       in T.replicate leftPadding (T.singleton c) <> t <> T.replicate rightPadding (T.singleton c)

artboxesRelativePath :: FilePath
artboxesRelativePath = "data/artboxes/"

{- | Load every AsciiBox by globbing the artbox directory and return it as a list with the associated file path.

-}
loadAllAsciiBoxes :: FilePath -> IO [(FilePath, AsciiBoxConfig)]
loadAllAsciiBoxes projectRoot = do
  let artboxesPath = projectRoot </> artboxesRelativePath
  files <- listDirectory artboxesPath
  let artboxFiles = filter (".abx" `isSuffixOf`) files
  forM artboxFiles $ \file -> do
    let filePath = artboxesPath </> file
    result <- parseAsciiBox filePath
    case result of
      Left err -> error $ "Error parsing " ++ filePath ++ ": " ++ show err
      Right config -> return (filePath, config)


loadAsciiBox :: FilePath -> FilePath -> IO AsciiBoxConfig
loadAsciiBox projectRoot path = do
  result <- parseAsciiBox $ projectRoot <> artboxesRelativePath <> path
  case result of
    Left err -> error $ "Error parsing TOML: " ++ show err
    Right box -> pure box

-- Main function for demonstration (parse the TOML and apply the container)
containerize :: FilePath -> FilePath -> Text -> IO Text
containerize projectRoot fileName body = do
    -- Parse the TOML file to get the AsciiBox
    box <- loadAsciiBox projectRoot fileName
    -- Apply the container and print the final result
    pure $ applyContainer box body