-- | Special text formatting functions.
--
-- Mostly gets used as Mustache lambdas.
module TextUtils where

import Data.List

import qualified Data.Text as Text

import qualified Text.Layout.Table.Justify as Justify
import Text.Wrap


-- should hyphenate break long words
justify2 :: Text.Text -> Text.Text
justify2 text = Text.pack . unlines $ Justify.justifyText paragraphWidth (unlines wrappedLines)
 where
  paragraphWidth = 60
  wrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }
  wrappedLines = map Text.unpack $ wrapTextToLines wrapSettings paragraphWidth text


justify' :: Text.Text -> Text.Text
justify' text = Text.pack . unlines $ justify (Text.unpack text)


justify :: String -> [String]
justify string = map (flip addSpaceToNthWord 0) wrappedLines
 where
  paragraphWidth = 60
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
