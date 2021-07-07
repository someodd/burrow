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
module FrontMatter (toVariablePairs, getFrontMatter, FrontMatter(..), FileFrontMatter) where

import Data.Maybe (fromMaybe)
import Data.Hourglass as HG
import Errata
import NeatInterpolation (text)
import qualified Data.Vector as V
import qualified Data.Dates.Parsing as DP
import qualified Data.Map as Map
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.ByteString as ByteString
import Data.Frontmatter (Parser, IResult(..), frontmatter, parse)
import Data.Yaml (YamlException(..), YamlMark(..), ParseException(..), prettyPrintParseException, ParseException, FromJSON, parseJSON, withObject, (.:?), decodeEither', Value(..))
import Data.Text.Encoding         as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Types (ContentType(..))
import Config (getConfig, getConfigValue)


getConfigTime :: IO (String)
getConfigTime = do
  config <- getConfig
  timeFormat <- getConfigValue config "general" "timeFormat"
  pure $ timeFormat


-- FIXME: not great implementation.
-- | An error encountered while attempting to parse frontmatter.
data FrontMatterError = FrontMatterError T.Text

instance Show FrontMatterError where
  show (FrontMatterError t) = T.unpack t


tagParseError' :: T.Text -> FilePath -> T.Text -> String
tagParseError' frontMatterRaw filePath parseError = do
  TL.unpack . prettyErrors frontMatterRaw $ [
    Errata
      { errataHeader = Just "Frontmatter Tag Format Error"
      , errataBlocks = [Block fancyRedStyle (filePath, 1, 1) Nothing [] (Just $ T.pack $ unlines . map ("| " <>) . lines $ T.unpack frontMatterRaw)]
      , errataBody = Just parseError
      }
    ]


contentTypeParseError :: T.Text -> FrontMatterError
contentTypeParseError badRenderAs = FrontMatterError $ [text|
There was a problem in your frontmatter!

You defined this `renderAs`:

  $badRenderAs

The only valid values for `renderAs` are:

  * menu: for gophermaps/menus in gopherspace
  * file: for regular text files in gopherspace

Suggestions:

  * Remove the `renderAs`
  * Set the `renderAs` to a valid value as described above
|]


dateTimeParseError :: T.Text -> T.Text -> FrontMatterError
dateTimeParseError declaration parseAttempt = FrontMatterError $ [text|
There was a problem in your frontmatter!

I couldn't figure out a way to create a valid date format (like ISO8601)
out of this format you gave me for "$declaration":

  $parseAttempt

Suggestions:

  * Try a date format like YYYY-MM-DD
  * Just remove the "$declaration" specification altogether
|]


-- FIXME: needs to define the file the error was encountered in.
-- this whole thing is hacky. Could even use mustache!
tagParseError :: T.Text -> FrontMatterError
tagParseError value = FrontMatterError $ [text|
Couldn't parse frontmatter because of incorrect JSON/YAML type for
"tags."

The value you supplied for "tags" was parsed into this type we weren't looking
for:

  $value.

The problem is with your incorrect "tags" declaration. Instead,
"tags" has to be either a JSON/YAML array like this:

  ---
  tags: [recipes, bread]
  ---

... or a space-separated JSON/YAML string like this:

  ---
  tags: recipes bread
  ---

|]


prettyPrintYamlException :: T.Text -> FilePath -> T.Text -> T.Text -> (Int, Int, Int) -> String
prettyPrintYamlException frontMatterRaw filePath problem context (_, line, column) = do
  let errata = Errata
                 { errataHeader = Just "Your YAML/frontmatter is incorrect!"
                 , errataBlocks = [Block fancyRedStyle (filePath, line+1, column + T.length context) Nothing [Pointer (line+1) column (column+1) False (Just problem)] Nothing]
                 , errataBody = Just context
                 }
  TL.unpack . prettyErrors frontMatterRaw $ [errata]


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
  -- FIXME ^ should error if not recognized type? catches typos.
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
  , fmParentTemplate :: Maybe FilePath
  -- ^ Embed the file inside another template by the path mentioned. If set,
  -- this setting will override the setting derived from file extension name.
  , fmRenderAs :: Maybe ContentType
  -- ^ Can render as "file" or "menu." Overrides settings derived from file name.
  } deriving (Show)


-- | Create a list of pairs of (entry name, value) for all the relevant information
-- one might want to include as a string in a template, file, or post.
toVariablePairs :: FrontMatter -> IO [(T.Text, T.Text)]
toVariablePairs frontMatter = do
  timeFormat <- getConfigTime
  let toConfigTime = T.pack . HG.timePrint timeFormat
  pure $ foldr addVar []
    [ ("title", fmTitle)
    , ("tags", \x -> fmTags x >>= Just . T.intercalate ", ")
    , ("author", fmAuthor)
    , ("published", \x -> fmPublished x >>= Just . toConfigTime)
    , ("updated", \x -> fmUpdated x >>= Just . toConfigTime)
    ]
 where
   addVar (name, entry) acc = fromMaybe [] (entry frontMatter >>= \x -> Just [(name, x)]) ++ acc


-- | Allows for the decoding of ByteString into the `FrontMatter` type.
instance FromJSON FrontMatter where
  parseJSON = withObject "FrontMatter" $ \o -> FrontMatter
    -- FIXME: need to have a fail in the parse context here just like with tags, for if dateStringToDateTime fails!
    <$> (dateTime "published" =<< (o .:? "published" :: AT.Parser (Maybe Value)) :: AT.Parser (Maybe DP.DateTime))
    <*> (dateTime "updated" =<< (o .:? "updated"))
    <*> o .:? "title"
    <*> o .:? "author"
    -- Sorry for this ugly bit, I wanted to be explicit to make it more readable.
    <*> ((\x -> either (fail . show) pure (traverse tagList x :: Either FrontMatterError (Maybe [T.Text]))) =<< (o .:? "tags" :: AT.Parser (Maybe Value)))
    <*> o .:? "type"
    <*> o .:? "variables"
    <*> o .:? "skipMustache"
    <*> o .:? "skipMarkdown"
    <*> o .:? "parentTemplate"
    <*> (o .:? "renderAs" >>= \maybeInsideParser -> either (fail . show) pure (traverse makeRenderAs maybeInsideParser))
   where
    -- | Make the renderAs front matter value into a `ContentType` or error if the
    -- value is unrecognized.
    makeRenderAs :: Value -> Either FrontMatterError ContentType
    makeRenderAs (String "menu") = Right GopherMenuType
    makeRenderAs (String "file") = Right GopherFileType
    makeRenderAs someValue = Left $ contentTypeParseError (T.pack . show $ someValue)

    -- | Convert a parser value into a datetime object or fail!
    --
    -- For a monadic operation inside parser. The bind operation from (o .:? "publish")
    -- for example puts us inside the `AT.Parser` monad and the type of "publish" expects
    -- a `Maybe DP.DateTime` and the monad operation expects an `AT.Parser a`.
    dateTime :: T.Text -> Maybe Value -> AT.Parser (Maybe DP.DateTime)
    dateTime declaration (Just (String dateText)) =
      let v = case DP.extractDateTimesY 2021 (T.unpack dateText) of
                [] -> Left $ dateTimeParseError declaration dateText
                a:_ -> Right $ Just a
      -- This line will return either a monad failure inside the `AT.Parser` monad
      -- context, using its `MonadFail` instance, or it will simply give the
      -- `Right` value (Just some date time) inside the AT.Parser with `pure`.
      in either (fail . show) pure v
    dateTime _ _ = pure Nothing

    -- | Create a list of tags from either a space separated string of tags or
    -- from a list of tags.
    tagList :: Value -> Either FrontMatterError [T.Text]
    tagList (String text') = Right $ T.words text'
    tagList (Array array) = Right $ map (\(String text') -> text') $ V.toList array
    -- FIXME: bad error
    tagList v = Left $ tagParseError (T.pack . show $ v)


-- | The parser to be used by `parse`. Works with `FrontMatter`'s `FromJSON`
-- instance through type inference.
--
-- The aeson object in the frontmatter is given an explicit type required for
-- the parser to return anything with a FromJSON instance.
frontmatterBurrow :: FilePath -> Parser FrontMatter
frontmatterBurrow filePath =
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
      Left (AesonException parseException) -> error $ tagParseError' (T.decodeUtf8 parserByteString) filePath (T.pack parseException)
      Left (InvalidYaml (Just (YamlParseException {yamlProblem=problem, yamlContext=context, yamlProblemMark=(YamlMark indx line column)}))) -> do
        let frontMatterRaw = T.decodeUtf8 parserByteString
        error $ prettyPrintYamlException frontMatterRaw filePath (T.pack problem) (T.pack context) (indx, line, column)
      Left anotherParseException -> error . prettyPrintParseException $ anotherParseException
      -- We leave the Parser monad by ending with a Parser FrontMatter, since
      -- decodeEither' will use the parseJSON method for `FrontMatter`.
      Right frontMatter -> return frontMatter:: Parser FrontMatter


-- | If the `FrontMatter` can be retreived, return it, along with the rest of
-- the document with the Frontmatter removed. If there was no frontmatter
-- the document is returned as the second item in the pair, still.
getFrontMatter :: FilePath -> T.Text -> (Maybe FrontMatter, T.Text)
getFrontMatter filePath text' = do
  let bsText = T.encodeUtf8 text'
      result = parse (frontmatterBurrow filePath) bsText :: IResult ByteString.ByteString FrontMatter
  case result of
      Done documentWithoutFrontMatter frontMatter -> (Just frontMatter, T.decodeUtf8 documentWithoutFrontMatter :: T.Text)
      -- I honestly don't understand this part. Sometimes the parser will need
      -- more input so it can resume.
      Partial f -> case (f ByteString.empty) of
                     Done ri fm -> (Just fm, T.decodeUtf8 ri)
                     Partial _ -> error "wtf"
                     Fail _ _ _ -> error "bruh"
      -- Either a failure to parse the `FrontMatter`, or there was none!
      Fail _ _ _ -> (Nothing, text')
