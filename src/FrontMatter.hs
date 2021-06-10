-- | Implements YAML headers of plaintext files in order to specify
-- metadata, like tagging blog posts. This is modelled after Jekyll's
-- FrontMatter: https://jekyllrb.com/docs/front-matter/
--
-- Here's an example:
--
--   ---
--   tags: [foo, bar]
--   date: March 3rd, 2021 at 2:24pm
--   ---
--   Main content here.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module FrontMatter (getFrontMatter, FrontMatter(..), FileFrontMatter) where

--import Text.RawString.QQ (r)
import NeatInterpolation (text)
import qualified Data.Vector as V
import qualified Data.Dates.Parsing as DP
import qualified Data.Map as Map
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.ByteString as ByteString
import Data.Frontmatter (Parser, IResult(..), frontmatter, parse)
import Data.Yaml (prettyPrintParseException, ParseException, FromJSON, parseJSON, withObject, (.:?), decodeEither', Value(..))
import Data.Text.Encoding         as T
import qualified Data.Text as T


-- FIXME: not great implementation.
-- | An error encountered while attempting to parse frontmatter.
data FrontMatterError = FrontMatterError T.Text

instance Show FrontMatterError where
  show (FrontMatterError t) = T.unpack t

-- FIXME: needs to define the file the error was encountered in.
tagParseError :: T.Text -> T.Text -> T.Text -> FrontMatterError
tagParseError filePath frontMatter value = FrontMatterError $ [text|
Couldn't parse frontmatter in $filePath because of incorrect JSON/YAML type for
"tags."

You defined this frontmatter:

  $frontMatter

Specifically the problem is with your incorrect "tags" declaration. Instead,
"tags" has to be either a JSON/YAML array like this:

  ---
  tags: [recipes, bread]
  ---

... or a space-separated JSON/YAML string like this:

  ---
  tags: recipes bread
  ---

The value you supplied for "tags" was parsed into this type we weren't looking
for:

  $value.
|]


-- | A filepath and its associated filematter meta (if any).
type FileFrontMatter = (FilePath, Maybe FrontMatter)

-- | Representation of the FrontMatter found in a file.
--
-- For frontmatter for phlogs see the `Phlog` module.
data FrontMatter = FrontMatter
  { fmPublished :: Maybe DP.DateTime
  -- ^ When the post was originally created.
  , fmUpdated :: Maybe DP.DateTime
  -- ^ When the post was last revised/updated. 
  , fmTitle :: Maybe T.Text
  -- ^ The document name/title.
  , fmAuthor :: Maybe T.Text-- FIXME: turn into a list?
  -- ^ The person who created the document.
  , fmTags :: Maybe [T.Text]-- FIXME: remove Maybe because [] indicates no tags, anyway?
  -- ^ What a post is tagged with. Tags impact which tag indexes (see the
  -- `Phlog` module) the post shows up in.
  --
  -- Can be defined in the frontmatter as a list:
  --
  --    tags: [travel, europe]
  --
  -- ... or as a space-separated string of tags:
  --
  --    tags: travel europe
  --
  , fmType :: Maybe T.Text
  -- ^ Only type supported right now is "post" if defined at all.
  -- Defining it as "post" will put it into the phlog index
  -- system defined in the `Phlog` module.
  , fmVariables :: Maybe (Map.Map T.Text T.Text) -- FIXME: maybe fmSubstitutions would be a better name?
  -- ^ Allows you to set Mustasche substitutions for the final rendered product.
  -- For example, if you define:
  --
  --    variables: {"someVariable": "hello, world!"}
  --
  -- ... in your frontmatter, then any instance of {{ someVariable }}, in a partial
  -- or directly in the post itself will be replaced with "hello, world!"
  , fmSkipMustache :: Maybe Bool
  -- ^ Do not use the Mustache parser for this file (if true).
  , fmSkipMarkdown :: Maybe Bool
  -- ^ Do not use the Markdown parser for this file (if true).
  } deriving (Show)


-- FIXME: could feasibly use Reader to get the date/time if it's passed from the building
-- process! Not sure if I care that much about it.
-- | Allows for the decoding of ByteString into the `FrontMatter` type.
instance FromJSON FrontMatter where
  parseJSON = withObject "FrontMatter" $ \o -> FrontMatter
    <$> (fmap (>>= dateStringToDateTime 2021) $ o .:? "published")
    <*> (fmap (>>= dateStringToDateTime 2021) $ o .:? "updated")
    <*> o .:? "title"
    <*> o .:? "author"
    -- TODO: for tags I want to be able to create a list out space-separated tags as well!
    <*> (either (fail . show) pure . traverse tagList =<< (o .:? "tags"))
    <*> o .:? "type"
    <*> o .:? "variables"
    <*> o .:? "skipMustache"
    <*> o .:? "skipMarkdown"
   where
    tagList :: Value -> Either FrontMatterError [T.Text]
    tagList (String text') = Right $ T.words text'
    tagList (Array array) = Right $ map (\(String text') -> text') $ V.toList array
    -- FIXME: bad error
    tagList v = Left $ tagParseError "" "" (T.pack . show $ v)

-- | The parser to be used by `parse`. Works with `FrontMatter`'s `FromJSON`
-- instance through type inference.
--
-- The aeson object in the frontmatter is given an explicit type required for
-- the parser to return anything with a FromJSON instance.
frontmatterBurrow :: Parser FrontMatter
frontmatterBurrow =
  -- The <?> allows us to name the parser we've created with frontmatterYaml'
  -- in the event that the parser fails.
  frontmatterYaml' <?> "frontmatterBurrow"
 where
  frontmatterYaml' = do
    -- We put ourselves into the Parser monad.
    parserByteString <- frontmatter :: Parser ByteString.ByteString
    -- This is where the magic happens. Decodes the `ByteString` using the
    -- `FromJSON` instance of the `FrontMatter` type because of the type singatures
    -- we supplied for this function.
    case (decodeEither' (parserByteString :: ByteString.ByteString) :: Either ParseException FrontMatter) of
      Left parseException -> error . prettyPrintParseException $ (parseException :: ParseException)
      -- We leave the Parser monad by ending with a Parser FrontMatter, since
      -- decodeEither' will use the parseJSON method for `FrontMatter`.
      Right frontMatter -> return frontMatter:: Parser FrontMatter


-- | If the `FrontMatter` can be retreived, return it, along with the rest of
-- the document with the Frontmatter removed.
getFrontMatter :: T.Text -> (Maybe FrontMatter, T.Text)
getFrontMatter text' = do
  let bsText = T.encodeUtf8 text'
      result = parse frontmatterBurrow bsText :: IResult ByteString.ByteString FrontMatter
  case result of
      Done documentWithoutFrontMatter frontMatter -> (Just frontMatter, T.decodeUtf8 documentWithoutFrontMatter :: T.Text)
      -- I honestly don't understand this part. Sometimes the parser will need
      -- more input so it can resume.
      Partial f -> case (f ByteString.empty) of
                     Done ri fm -> (Just fm, T.decodeUtf8 ri)
                     Partial _ -> error "wtf"
                     Fail _ _ errorMsg -> error errorMsg
      -- Either a failure to parse the `FrontMatter`, or there was none!
      Fail _ _ _ -> (Nothing, text')


-- FIXME: add fancy error for this and also make a part of frontmatter and automatically transform into this type?
-- remember: you don't HAVE to have a date.
-- FIXME: could error out (usage of `head`)
-- | Fuzzy match a date time string (for a `FrontMatter` date/time definition).
--
-- `extractDateTimesY` does all the heavy lifting.
--
-- >>> dateStringToDateTime (2021 :: Integer) (T.pack "july 29")
-- DateTime {dtDate = Date {dateYear = 2021, dateMonth = July, dateDay = 29}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}
-- >>> dateStringToDateTime (2021 :: Integer) (T.pack "2021-06-20T04:30")
-- DateTime {dtDate = Date {dateYear = 2021, dateMonth = June, dateDay = 20}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}
dateStringToDateTime :: Integer -> T.Text -> Maybe DP.DateTime
dateStringToDateTime defaultYear dateText = Just $ head $ DP.extractDateTimesY (fromIntegral defaultYear :: Int) (T.unpack dateText)
