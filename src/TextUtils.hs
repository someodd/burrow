-- TODO: maybe columnate stuff should get its own TextUtils.Columnate function!
-- | Special text formatting functions to make pretty files and menus in gopherspace.
--
-- Mostly gets used as Mustache lambdas.
{-# LANGUAGE OverloadedStrings          #-}
module TextUtils
  ( justify2
  , justify'
  , columnate2
  )
where

import Data.List
import Data.List.Split (chunksOf)

import qualified Data.Text as Text

import qualified Text.Layout.Table.Justify as Justify
import Text.Wrap


-- | The maximum width of the gopherhole page. All the functions use this
-- to do any width calculations.
maxWidth :: Int
maxWidth = 79


-- TODO: should hyphenate break long words
-- | Mustache-lambda-friendly function for text justification.
justify2 :: Text.Text -> Text.Text
justify2 text = Text.pack . unlines $ Justify.justifyText paragraphWidth (unlines wrappedLines)
 where
  paragraphWidth = maxWidth
  wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
  wrappedLines = map Text.unpack $ wrapTextToLines wrapSettings paragraphWidth text


-- | The Mustache-lambda-friendly version of Burrow's custom text justification function.
justify' :: Text.Text -> Text.Text
justify' text = Text.pack . unlines $ justify (Text.unpack text)


-- | Burrow's custom text justification function. Implemented for fun.
justify :: String -> [String]
justify string = map (flip addSpaceToNthWord 0) wrappedLines
 where
  paragraphWidth = maxWidth
  wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
  wrappedLines = map Text.unpack $ wrapTextToLines wrapSettings paragraphWidth (Text.pack string)

  -- FIXME: redocument, rename, clean up, refactor this
  addSpaceToNthWord :: String -> Int -> String
  addSpaceToNthWord l@(_:_) i
    | paragraphWidth == length l = l
    | paragraphWidth <= length l = l  -- FIXME: this condition shouldn't be happening (the less-than part)
    | otherwise =
        let newI = i `mod` (length $ words l)
            worded = spaceSplitter l
            -- this is wrong need different way to split words so it includes spaces duh
            wordedAdded = intercalate "" (take newI worded ++ [worded !! newI ++ " "] ++ drop (newI + 1) worded)
        in addSpaceToNthWord wordedAdded (i+1)
  addSpaceToNthWord [] _ = error "Can't add space to empty list!"

  -- FIXME: it's faster to build your lists up back-to-front, which would also remove the need for map reverse.
  -- instead of x ++ [n], use n : x (which is equivalent to [n] ++ x).
  --
  -- Split a string based off of a space (or group of spaces), while preserving
  -- the delimiter (n-spaces until a nonspace character occurs).
  --
  -- This allows for a version of the `words` function which preserves the
  -- amount of space between words (which is lumped in with the first of those
  -- two words).
  spaceSplitter :: String -> [String]
  spaceSplitter string' = map reverse (foldr func [] string')
    where
      func :: Char -> [String] -> [String]
      func n [] = [[n]]
      func n acc@(x:xs)
        -- If we're encountering a new nonspace after the last in the current
        -- group was a space then start a new group!
        | n /= ' ' && last x == ' ' = [n] : acc
        -- Otherwise, it follows, we're encountering something to add to the current group.
        | otherwise = (x ++ [n]) : xs


-- | Mustache-lambda-friendly version of the columnate function, meaning it has preconfigured
-- column width, max width, and lines per column.
columnate2 :: Text.Text -> Text.Text
columnate2 text = "\n\n" <> columnate 38 maxWidth 20 text <> "\n\n"


-- | This is used to separate columns vertically. The column gutter.
columnSeparator :: Text.Text
columnSeparator = " │ "


-- FIXME: needs to go more in depth talking about which each little function does in terms of twisting the text and columns around.
-- | Create columnated text out of a text block by defining the column width
-- and the total width (the "page" width), as well as the maximum number of lines
-- before splitting off onto a new "page" (set of columns).
columnate :: Int -> Int -> Int -> Text.Text -> Text.Text
columnate columnWidth totalMaxWidth maxLinesPerColumn textBlock = finalResult
 where
  -- 1: The starting point: create a single justified column. This is accomplished by
  -- doing word wrap and then justifying the text (as well as performing some padding,
  -- to ensure each line of the column is the exact same length, so when we put the
  -- columns side-by-side for each row of columns [a "page"] it looks proper).
  asSingleColumn :: Text.Text
  asSingleColumn =
    let wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True } -- It's not breaking long words... should start using knuth's algo
        wrapped = wrapTextToLines wrapSettings columnWidth textBlock
    in Text.unlines $ map (Text.justifyLeft columnWidth ' ') $ map Text.pack $ Justify.justifyText columnWidth (Text.unpack . Text.unlines $ wrapped)

  -- 2: Group the single justified column into text chunks of maxLinesPerColumn
  maxLinesGroups :: [[Text.Text]]
  maxLinesGroups =
    let singleColumnLines = Text.lines asSingleColumn
    in chunksOf maxLinesPerColumn singleColumnLines

  -- 3: Take the single justified column, which has been broken up into [Text.Text] groups of a max of
  -- maxLinesPerColumn number of elemnts and further group those into "pages."
  --
  -- A "page" is just a row of columns. The number of columns in a row is determined by the totalMaxWidth
  -- and the columnWidth (in order to tell how many columns fit given that criteria).
  groupedByPage :: [[[Text.Text]]]
  groupedByPage =
    let howManyColumnsFit = totalMaxWidth `quot` (columnWidth + 1) -- the +1 is for the column spacing
    in chunksOf howManyColumnsFit maxLinesGroups

  -- 4: Join the columns together to produce a Text!
  --
  -- This will "transpose" each column row, in order to make it appear as if each column is displayed
  -- side-by-side in a given row.
  --
  -- Remember, we're going from a list of "pages," where pages are
  -- a row of columns (represented as a list of Text).
  --
  -- Columns are separated here with the columnSeparator.
  finalResult :: Text.Text
  finalResult =
    let parsePage page = Text.unlines $ (map (Text.intercalate columnSeparator)) $ transpose $ page
    in Text.unlines $ map parsePage groupedByPage
