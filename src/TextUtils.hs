-- | Special text formatting functions.
--
-- Mostly gets used as Mustache lambdas.
{-# LANGUAGE OverloadedStrings          #-}
module TextUtils where

import Data.List
import Data.List.Split (chunksOf)

import qualified Data.Text as Text

import qualified Text.Layout.Table.Justify as Justify
import Text.Wrap


maxWidth = 79

-- should hyphenate break long words
justify2 :: Text.Text -> Text.Text
justify2 text = Text.pack . unlines $ Justify.justifyText paragraphWidth (unlines wrappedLines)
 where
  paragraphWidth = maxWidth
  wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
  wrappedLines = map Text.unpack $ wrapTextToLines wrapSettings paragraphWidth text


justify' :: Text.Text -> Text.Text
justify' text = Text.pack . unlines $ justify (Text.unpack text)


justify :: String -> [String]
justify string = map (flip addSpaceToNthWord 0) wrappedLines
 where
  paragraphWidth = maxWidth
  wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
  wrappedLines = map Text.unpack $ wrapTextToLines wrapSettings paragraphWidth (Text.pack string)

  -- should use guards to check for bad cases or be more safe idk
  addSpaceToNthWord :: String -> Int -> String
  addSpaceToNthWord l@(a:ax) i
    | paragraphWidth == length l = l
    | paragraphWidth <= length l = l
    | otherwise =
        let newI = i `mod` (length $ words l)
            worded = spaceSplitter l
            -- this is wrong need different way to split words so it includes spaces duh
            wordedAdded = intercalate "" (take newI worded ++ [worded !! newI ++ " "] ++ drop (newI + 1) worded)
        in addSpaceToNthWord wordedAdded (i+1)
  addSpaceToNthWord [] i = error "Can't add space to empty list!"

  spaceSplitter :: String -> [String]
  spaceSplitter string = map reverse (foldr func [] string)
    where
      func :: Char -> [String] -> [String]
      func n [] = [[n]]
      func n acc@(x:xs)
        -- if we're encountering a new nonspace after a space then start a new group
        | n /= ' ' && last x == ' ' = [n] : acc
        -- if we're encountering a space add to current group
        | n == ' ' = (x ++ " ") : xs
        -- if we're encountering a nonspace simply add to current group
        | n /= ' ' = (x ++ [n]) : xs


-- This doesn't preserve the \n
columnate2 text = "\n\n" <> columnate 38 maxWidth 20 text <> "\n\n"

{-
split into lines after get single column
group into text blocks of n lines/size
transpose? so the first line in one block is aside the first line of the next column and so forth?
join/intercalate
-}
-- | Create columnated text out of a text block by defining the column width
-- and the total width, as well as the maximum number of lines before splitting
-- off into a new page.
--columnate :: Int -> Int -> Int -> Text.Text -> Text.Text
columnate columnWidth totalMaxWidth maxLinesPerColumn textBlock = finalResult
 where
  -- The starting point: create a single justified column
  -- NOTE: not only must we wrap, but we have a special job where we must also pad
  asSingleColumn :: Text.Text
  asSingleColumn =
    let wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True } -- It's not breaking long words... should start using knuth's algo
        wrapped = wrapTextToLines wrapSettings columnWidth textBlock
    in Text.unlines $ map (Text.justifyLeft columnWidth ' ') $ map Text.pack $ Justify.justifyText columnWidth (Text.unpack . Text.unlines $ wrapped)

  -- Group the single justified column into text chunks of maxLinesPerColumn
  maxLinesGroups :: [[Text.Text]]
  maxLinesGroups =
    let singleColumnLines = Text.lines asSingleColumn
    in chunksOf maxLinesPerColumn singleColumnLines

  -- now group the maxLinesGroup by the max # of columns
  howManyColumnsFit = totalMaxWidth `quot` (columnWidth + 1) -- the +1 is for the column spacing
  groupedByPage = chunksOf howManyColumnsFit maxLinesGroups

  -- now balh
  parsePage page = Text.unlines $ (map (Text.intercalate (Text.pack " | "))) $ transpose $ page

  nicer = map parsePage groupedByPage

  -- split into groups of how many columns fit to create "pages" or whatever

  -- now...
  --finalResult = putStr $ Text.unpack $ Text.unlines $ (map (Text.intercalate (Text.pack " @ "))) $ transpose $ maxLinesGroups
  finalResult = Text.unlines $ nicer
