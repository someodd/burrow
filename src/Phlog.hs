{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
module Phlog
  (
  -- * Produce different kinds of indexes for the phlog.
  -- $phlogIndexes
    renderMainPhlogIndex
  , renderTagIndexes
  -- * Re-exports to make handling phlogs easier.
  , FrontMatter(..)
  , getFrontMatter
  , FileFrontMatter
  ) where

import Data.Hourglass as HG
import Data.Default (def)
import qualified Text.XML as XML
import Text.XML.Writer
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Dates.Parsing as DP
import System.FilePath (splitExtension, takeDirectory, (</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
--import Control.Arrow ((&&&))
import Control.Monad.Reader (runReader, Reader, ask)
import Data.List (sortOn, intercalate, isSuffixOf)
import qualified Data.Text as T

import Config (getConfig, getConfigValue)
import FrontMatter (FileFrontMatter, FrontMatter(..), getFrontMatter)


-- | The Atom spec wants rfc3339 (ISO8601 as far as I can tell) datetime strings.
toRFC3339 :: DP.DateTime -> T.Text
toRFC3339 = T.pack . HG.timePrint HG.ISO8601_DateAndTime


-- | This is like `FrontMatter`, except blog posts require certain metadata,
-- largely to be indexed properly, to be sortable, and to be capable of being
-- put into atom/feed format.
--
-- To get all the info on what each record means look at `FrontMatter`.
data PostMeta = PostMeta
  { metaPublished :: DP.DateTime
  , metaUpdated :: DP.DateTime-- FIXME: use Data.Default so it can default to metaPublished.
  -- ^ Defaults to metaPublished.
  , metaPath :: T.Text
  -- ^ File path (relative) to the post.
  , metaTitle :: T.Text
  , metaAuthor :: Maybe T.Text
  -- ^ Can be Nothing because of default author entry in the config `ini`.
  , metaTags :: Maybe [T.Text]
  , metaFrontMatter :: FrontMatter
  -- ^ All of the FrontMatter.
  } deriving (Show)

-- FIXME: need beautiful errors informing a user of what is needed.
-- only trigger this if it is post type?
pairToPostMeta :: FileFrontMatter -> Maybe PostMeta
pairToPostMeta (filePath, Just frontMatter) = Just $ PostMeta
  { metaPublished = fromJust $ fmPublished frontMatter
  , metaUpdated = fromMaybe (fromJust $ fmPublished frontMatter) (fmUpdated frontMatter)
  , metaPath = T.pack filePath
  , metaTitle = fromJust $ fmTitle frontMatter
  , metaAuthor = fmAuthor frontMatter
  , metaTags = fmTags frontMatter
  , metaFrontMatter = frontMatter
  }
pairToPostMeta (_, Nothing) = Nothing

-- | Filters out non-posts based on the type: post.
preparePostsOnlyFromPairs :: [FileFrontMatter] -> [PostMeta]
preparePostsOnlyFromPairs filePathFrontMatterPairs = do
  [postMeta | pair@(_, Just frontMatter) <- filePathFrontMatterPairs, fromMaybe False (fmType frontMatter >>= \t -> Just $ t == "post"), let (Just postMeta) = pairToPostMeta pair]


data PhlogConfig = PhlogConfig
  { phlogPath :: FilePath
  -- ^ Path to the phlog directory.
  , phlogTagPath :: FilePath
  -- ^ Path to the phlog's tags directory.
  , phlogDefaultAuthor :: String
  -- ^ The name of the author that is used by default.
  , phlogHost :: String
  -- ^ The domain name/host/ip address of the gopherhole.
  , phlogPort :: String
  -- ^ The port of the gopherhole.
  }

getPhlogConfig :: IO PhlogConfig
getPhlogConfig = do
  configParser <- getConfig
  -- phlog section
  phlogPath' <- getConfigValue configParser "phlog" "phlogPath"
  tagPath' <- getConfigValue configParser "phlog" "tagPath"
  defaultAuthor' <- getConfigValue configParser "phlog" "defaultAuthor"
  -- general section
  host' <- getConfigValue configParser "general" "host"
  port' <- getConfigValue configParser "general" "port"
  pure $ PhlogConfig
    { phlogPath = phlogPath'
    , phlogTagPath = tagPath'
    , phlogDefaultAuthor = defaultAuthor'
    , phlogHost = host'
    , phlogPort = port'
    }


data AtomFeedRecipe = AtomFeedRecipe
  { atomTitle :: T.Text
  -- ^ The title of the feed.
  , atomEntries :: [PostMeta]
  -- ^ All the individual entries for the feed.
  , atomPhlogConfig :: PhlogConfig
  -- ^ All of the information from the config required to create the feed.
  , atomPath :: FilePath
  -- ^ Path (not URI) to this feed. Is also used for the feed ID and its self href.
  }

data AtomFeedEntryRecipe = AtomFeedEntryRecipe
  { entryPostMetas :: [PostMeta]
  , entryPhlogConfig :: PhlogConfig
  }


-- | Get the URI which points to this gopherhole using the phlog config info.
--
-- >>> :{
--  let
--    phlogConfig =
--      PhlogConfig
--        { phlogPath="phlog/"
--        , phlogTagPath="tag/"
--        , phlogDefaultAuthor="foo"
--        , phlogHost="example.org"
--        , phlogPort="70"
--        }
--  in baseURL phlogConfig
-- :}
-- "gopher://example.org:70/"
baseURL :: PhlogConfig -> String
baseURL phlogConfig =
  "gopher://" ++ phlogHost phlogConfig ++ ":" ++ phlogPort phlogConfig ++ "/"

instance ToXML AtomFeedEntryRecipe where
  toXML (AtomFeedEntryRecipe { entryPostMetas=postMetas, entryPhlogConfig=phlogConfig }) = do
    -- FIXME: needs to iterate and needs to not assume Just
    foldr ((>>) . assembleEntry) empty postMetas
   where
    -- These could be combined easily due to maybe monad...
    assembleEntry postMeta = do
     let postURI = (T.pack $ baseURL phlogConfig ++ (T.unpack $ metaPath postMeta))
     element "entry" $ do
       element "title" $ content (metaTitle postMeta)
       elementA "link" [("href", postURI)] empty
       element "id" $ content postURI
       element "author" $ element "name" $ content (T.pack $ phlogDefaultAuthor phlogConfig)
       fromMaybe empty (metaTags postMeta >>= \t -> Just $ elementA "category" [("term", T.intercalate " " t)] empty)
       element "published" $ content (toRFC3339 $ metaPublished postMeta)
       element "updated" $ content (toRFC3339 $ metaUpdated postMeta)


-- FIXME: need to handle no entries.
createAtomFeed :: AtomFeedRecipe -> XML.Document
createAtomFeed atomFeedRecipe = do
  let phlogConfig = atomPhlogConfig atomFeedRecipe
      base = baseURL phlogConfig
  atomDocument $ do
    element "title" $ content (atomTitle atomFeedRecipe)
    elementA "link" [("href", T.pack base)] $ empty
    elementA "link" [("href", T.pack $ base ++ (atomPath atomFeedRecipe)), ("rel", "self")] $ empty
    elementA "generator" [("uri", "https://github.com/hyperrealgopher/burrow")] $ content "hyperrealgopher's burrow" 
    element "id" $ content (T.pack $ base ++ (atomPath atomFeedRecipe))
    element "updated" $ content lastUpdated--FIXME
    toXML $ AtomFeedEntryRecipe (atomEntries atomFeedRecipe) phlogConfig
    comment "that's it!"
 where
  -- FIXME: I noticed that this indicates an error where atomEntries is empty... that should be impossible!
  -- I think it's because of tagSummary! If tagSummary is blank.
  lastUpdated :: T.Text
  lastUpdated = case map metaUpdated (atomEntries atomFeedRecipe) of
                  [] -> "n/a"-- FIXME: this is a bad way to handle this error?
                  list -> toRFC3339 $ maximum list

  atomDocument children = XML.Document
    { XML.documentPrologue = XML.Prologue def def def
    , XML.documentRoot = XML.Element "feed" (Map.fromList [("xmlns","http://www.w3.org/2005/Atom")]) (render children)
    , XML.documentEpilogue = def
    }

-- | Represents different kinds of phlog indexes and the tools required to do
-- things with them.
--
-- Type `a` is what will create the model (type `b`) used in creating the
-- gophermap as well as the Atom feed.
class PhlogIndex a b | b -> a where
  -- | Sort the index model.
  sortIndexModel :: b -> b

  -- | Create the data model of the phlog index in question. Do not use this
  -- directly, simply define it.
  createIndexModel' :: a -> b

  -- | Do not override this. This is what you should use to actually
  -- create the index model.
  createIndexModel :: a -> b
  createIndexModel = sortIndexModel . createIndexModel'

  -- | Render the phlog index model file in order to be viewed as a gophermap
  -- in gopherspace.  This includes creating the string/file contents from the
  -- supplied model, which is then written to file.
  renderIndexGophermap :: b -> IO ()

  -- TODO:
  -- This could do more work or have two abstract/higher order functoins since
  -- all my renderatoms are basically the same and jsut spit out a document and
  -- then write the exact same way...
  -- could do a createAtom' b -> document then renderAtom Document -> IO but
  -- that complicates things because path needed
  renderAtom :: b -> IO ()

  -- | Render both the gophermap and the atom feed.
  renderAll :: b -> IO ()
  renderAll b = renderAtom b >> renderIndexGophermap b


-- FIXME: no need for reader anymore (year)
-- The Int is the current year
newtype MainPhlogIndex = MainPhlogIndex (Reader Integer [PostMeta])

-- | The main phlog index which contains all of the posts.
instance PhlogIndex [PostMeta] MainPhlogIndex where
  sortIndexModel (MainPhlogIndex readerIntPostMetas) = do
      MainPhlogIndex $ do
        -- FIXME
        _ <- ask
        fmap (sortOn metaUpdated) readerIntPostMetas

  createIndexModel' postMetasPairs = do
    MainPhlogIndex $ do
      _ <- ask
      pure $ postMetasPairs

  -- | Create the main phlog index which includes all the posts sorted by date.
  renderIndexGophermap (MainPhlogIndex mainPhlogIndex) = do
    currentYear <- getCurrentYear
    configParser <- getConfig
    buildPath <- getConfigValue configParser "general" "buildPath"
    phlogPath' <- getConfigValue configParser "phlog" "phlogPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let outputPath = buildPath </> phlogPath' </> ".gophermap"
        phlogIndex = runReader mainPhlogIndex currentYear
    writeFile outputPath $ makePhlogIndexPage phlogIndex tagPath
   where
    makePhlogIndexPage :: [PostMeta] -> FilePath -> String
    makePhlogIndexPage postMetas tagIndexPath =
      let viewByTagsEntry = show $ MenuLink "1" "view by tags" tagIndexPath Nothing Nothing
          allThePosts = "all phlog posts\n" ++ (intercalate "\n" $ map (makeLocalLink) postMetas)
      in viewByTagsEntry ++ "\n" ++ allThePosts

  renderAtom (MainPhlogIndex mainPhlogIndex) = do
    currentYear <- getCurrentYear
    phlogConfig <- getPhlogConfig
    let phlogIndex = runReader mainPhlogIndex currentYear
        -- FIXME
        atomFeed = (createAtomFeed $ AtomFeedRecipe {atomTitle="All Posts", atomEntries=phlogIndex, atomPhlogConfig=phlogConfig, atomPath="phlog/main.xml"})
    createDirectoryIfMissing True "built/phlog/"
    XML.writeFile def "built/phlog/main.xml" atomFeed


renderMainPhlogIndex :: [FileFrontMatter] -> IO ()
renderMainPhlogIndex pairs = do
  let mainPhlogIndex = createIndexModel (preparePostsOnlyFromPairs pairs) :: MainPhlogIndex
  renderAll mainPhlogIndex


-- | This is for the default date while interpreting FrontMatter dates.
getCurrentYear :: IO Integer
getCurrentYear = (\(y,_,_) -> y) <$> (getCurrentTime >>= return . toGregorian . utctDay) 


type Tag = T.Text


-- FIXME: do not need defaultYear anymore
-- | Sort [PostMeta] based on date.
sortOnDate :: Integer -> [PostMeta] -> [PostMeta]
sortOnDate _ = sortOn metaUpdated


newtype MainTagIndex = MainTagIndex (HashMap.HashMap Tag [PostMeta])

-- FIXME/TODO: there's a TON of overlap between this and the other tag indexes so write some helper functions
-- FIXME/TODO: just use Group like SpecificTagIndex does but for all
-- | The phlog index which lists all the tags and the latest posts for each tag.
instance PhlogIndex [PostMeta] MainTagIndex where
  -- | Sort the hashmap values by date.
  sortIndexModel (MainTagIndex mainTagIndexMap) =
    let defaultYear = 2021
    in MainTagIndex $ HashMap.map (sortOnDate defaultYear) mainTagIndexMap

  -- NOTE: due to PhlogIndex SpecificTagIndex being similar, this results in much overlap/recalculation...
  createIndexModel' postMetas =
    MainTagIndex $ HashMap.map (take postsPerTag) $ HashMap.fromListWith (++) result
   where
    -- TODO/FIXME: make into config value... will have to use reader?
    postsPerTag :: Int
    postsPerTag = 3

    result :: [ (Tag, [PostMeta]) ]
    result =
      [(tag, [postMeta]) | postMeta <- postMetas, tag <- fromMaybe [] (metaTags postMeta)]-- FIXME: what if no tags? that should error right and be just fine?

  -- TODO: rewrite better
  renderIndexGophermap (MainTagIndex mainTagIndex) = do
    -- get paths from config
    configParser <- getConfig
    -- FIXME: what if build path from CLI instead of INI? Also why not use getPhlogConfig?
    -- It also doesn't make sense that we have a --spacecookie flag but then we just use
    -- .gophermap by default everywhere.
    buildPath <- getConfigValue configParser "general" "buildPath"
    tagPath <- getConfigValue configParser "phlog" "tagPath"
    let mainTagIndexPath = buildPath </> tagPath </> ".gophermap"

    -- write out the main tag index
    createDirectoryIfMissing True (takeDirectory mainTagIndexPath)
    writeFile mainTagIndexPath (makeMainTagIndex tagPath)
   where
    allTags :: [T.Text]
    allTags = HashMap.keys mainTagIndex

    -- TODO: rewrite better
    -- | Write the index of tag indexes! This makes it so the user can see all the tags and
    -- see five example posts (should make this sorted by date...) should include instead of
    -- filepath include a bunch of data necessary for building the indexes.
    makeMainTagIndex :: FilePath -> String
    makeMainTagIndex tagIndexDirectory =
      -- fromjust is bad
      -- FIXME: this kleisli function is a hack because no fm was included in tag index at the moment
      let viewAllPostsEntry tag = show $ MenuLink "1" ("View all posts tagged " ++ (T.unpack tag)) (tagIndexDirectory ++ (T.unpack tag)) Nothing Nothing
          threeSummary tag = intercalate "\n"
            [ (T.unpack tag)
            -- FIXME: use of fromJust here is bad.
            , intercalate "\n" $ map makeLocalLink $ fromJust $ HashMap.lookup tag mainTagIndex
            , viewAllPostsEntry tag
            ]
      in intercalate "\n\n" $ map threeSummary allTags

  -- FIXME: hardcoded paths!
  -- FIXME/TODO: this is not doing what it should be! very sloppily put together...
  renderAtom (MainTagIndex mainTagIndexMap) = do
    phlogConfig <- getPhlogConfig
    config <- getConfig
    -- FIXME: what if CLI argument build path override?
    buildPath <- getConfigValue config "general" "buildPath"
    let phlogIndex = foldr ((++) . snd) [] $ HashMap.toList $ mainTagIndexMap
        phlogDirectory = phlogPath phlogConfig
        atomRelativePath = phlogDirectory </> "tagSummary.xml"
        atomFeed = (createAtomFeed $ AtomFeedRecipe "Tag Summaries" phlogIndex phlogConfig atomRelativePath) -- FIXME: path
    createDirectoryIfMissing True $ buildPath </> phlogDirectory
    XML.writeFile def (buildPath </> atomRelativePath) atomFeed


newtype SpecificTagIndex = SpecificTagIndex (Tag, [PostMeta])

instance PhlogIndex (PostMetasGroupPair Tag) SpecificTagIndex where
  sortIndexModel (SpecificTagIndex (tag, filePathFrontMatterPairs)) =
    let year = 2021
    in SpecificTagIndex $ (tag, sortOnDate year filePathFrontMatterPairs)

  -- FIXME: is all I need to do is sort postMetasPairs?
  createIndexModel' (PostMetasGroupPair (_, tag, postMetasPairs)) =
    -- FIXME
    SpecificTagIndex (tag, postMetasPairs)

  renderIndexGophermap (SpecificTagIndex (tag, specificTagIndexes)) = do
    configParser <- getConfig
    tagIndexPath <- getConfigValue configParser "phlog" "tagPath"
    buildPath <- getConfigValue configParser "general" "buildPath"
    let outputPath = buildPath </> tagIndexPath </> (T.unpack tag) </> ".gophermap"
        outputDirectoryPath = takeDirectory outputPath
    createDirectoryIfMissing True outputDirectoryPath
    writeFile outputPath (tagIndexContents tag)
   where
    -- FIXME: no need for second argument Tag
    tagIndexContents :: Tag -> String
    tagIndexContents _ =
      -- fromjust bad! FIXME
      (T.unpack tag) ++ "\n\n" ++ (intercalate "\n" $ map makeLocalLink specificTagIndexes)

  renderAtom (SpecificTagIndex (tag, phlogIndex)) = do
    phlogConfig <- getPhlogConfig
    config <- getConfig
    -- FIXME: what if output directory override cli
    outputDirectory <- getConfigValue config "general" "buildPath"
    let phlogDirectory = phlogPath phlogConfig
        feedRelativePath = phlogDirectory </> "tags" </> (T.unpack tag <.> "xml")
        outputPath = outputDirectory </> feedRelativePath
        metas = phlogIndex
        -- FIXME
        atomFeed = (createAtomFeed $ AtomFeedRecipe (T.pack $ (T.unpack tag) ++ " tag phlog feed") metas phlogConfig feedRelativePath)
    createDirectoryIfMissing True (takeDirectory outputPath)
    XML.writeFile def outputPath atomFeed


-- | Currently I just use this for grouping together the (group label [like
-- "tag"], actual group value [like "recipes"], and finally the list of
-- PostMetas belonging to the group).
newtype PostMetasGroupPair a = PostMetasGroupPair (String, a, [PostMeta]) -- this is what get accepted by the thingy builder

-- | A bunch of `PostMetas` that have been grouped together based on some criteria.
newtype PostMetasGroup a = PostMetasGroup (String, HashMap.HashMap a [PostMeta]) deriving (Show)


-- | Create a single `PostMetasGroupPair` using a key to do a lookup from the
-- results of grouping `PostMetas` together into a `PostMetasGroup`.
getPostMetasGroupPair :: (Eq a, Hashable a) => PostMetasGroup a -> a -> PostMetasGroupPair a
getPostMetasGroupPair (PostMetasGroup (label, hashMap)) key =
  -- FIXME: fromjust
  PostMetasGroupPair (label, key, fromJust $ HashMap.lookup key hashMap)


-- | Group posts together by some property of the `PostMeta`.
--
-- If you're not getting a value that is a list you can use Identity as the
-- return value. You can also rely on record functions that have a type of
-- `Maybe a`.
--
-- >>> import Data.Functor.Identity (Identity(..))
-- >>> frontMatterHashMapGroup postMetas ("title", Identity . metaTitle) :: PostMetasGroup T.Text
-- PostMetasGroup ("title",fromList [...,("Duplicate Post Title",[PostMeta {...metaPath = "duplicate-post-title2.txt", metaTitle = "Duplicate Post Title"...},PostMeta {...metaPath = "duplicate-post-title.txt", metaTitle = "Duplicate Post Title"...}]),...,...)
-- >>> frontMatterHashMapGroup postMetas ("author", metaAuthor) :: PostMetasGroup T.Text
-- PostMetasGroup ("author",fromList [("Computer Nerd",[PostMeta {...metaPath = "unix-release.txt", metaTitle = "Unix is Released Today!", metaAuthor = Just "Computer Nerd"...}]),("A Video Game Fan",[PostMeta {...metaPath = "mario-release.txt", metaTitle = "Mario is Released Today!", metaAuthor = Just "A Video Game Fan",...},PostMeta {...metaPath = "zelda-release.txt", metaTitle = "Zelda is Released Today!", metaAuthor = Just "A Video Game Fan",...}])])
-- >>> frontMatterHashMapGroup postMetas ("tag", \pm -> fromMaybe [] (metaTags pm)) :: PostMetasGroup Tag
-- PostMetasGroup ("tag",fromList [("computing",[PostMeta {...metaTitle = "Unix is Released Today!",...metaTags = Just ["computing"], ...}]),("games",[PostMeta {...metaTitle = "Mario is Released Today!",...metaTags = Just ["games","nintendo"],...},PostMeta {...metaTitle = "Zelda is Released Today!",...metaTags = Just ["games","nintendo"],...}]),("nintendo",[PostMeta {...metaTitle = "Mario is Released Today!"...metaTags = Just ["games","nintendo"],...},PostMeta {...metaTitle = "Zelda is Released Today!",...metaTags = Just ["games","nintendo"],...}])])
frontMatterHashMapGroup
  :: (Eq a, Hashable a, Foldable f)
  => [PostMeta]
  -> (String, PostMeta -> f a)
  -> PostMetasGroup a
frontMatterHashMapGroup postMetaList (groupName, groupFunction) =
  PostMetasGroup $ (groupName, HashMap.fromListWith (++) result)
 where
  result =
    -- FIXME: will group tags ([foo, bar]: postmetas)... logically it shouldn't, though...
    [ (group, [postMeta]) | postMeta <- postMetaList, group <- (toList $ groupFunction postMeta)]


-- | Render the tag indexes from a collection of file paths and their
-- associated `FrontMatter` (if any), which contains the tags for that file.
--
-- The tags are written out to the supplied `FilePath`.
renderTagIndexes :: [FileFrontMatter] -> IO ()
renderTagIndexes filePathFrontMatter = do
  let postMetaList = preparePostsOnlyFromPairs filePathFrontMatter
      ppmg@(PostMetasGroup (_, hashMap)) = (frontMatterHashMapGroup postMetaList ("tag", \pm -> fromMaybe [] (metaTags pm)) :: PostMetasGroup Tag)
      tagsFound = HashMap.keys hashMap
  -- Only render tag indexes if there are any tags to render.
  if length tagsFound > 0
     then do
      -- Render the specific tag indexes using the tags we found and apply the
      -- "renderAll" from the PhlogIndex SpecificTagIndex instance.
      traverse_ (\x -> renderAll (createIndexModel $ getPostMetasGroupPair ppmg x :: SpecificTagIndex)) tagsFound
      -- Render the main tag index (summary of tags).
      let mainTagIndex = createIndexModel (preparePostsOnlyFromPairs filePathFrontMatter) :: MainTagIndex
      renderAll mainTagIndex
     else pure ()


-- FIXME: pass the menu suffix here (file extension)
-- | A useful tool in making phlog indexes: create a nice link for the menu
-- using a supplied pair from PostMetas.
--
-- >>> makeLocalLink postMeta
-- "0Zelda is Released Today!\t/zelda-release.txt"
makeLocalLink :: PostMeta -> String
makeLocalLink postMeta
  -- FIXME: hardcoded "menu"
  | ".menu" `isSuffixOf` (fst . splitExtension $ path) = restOfLink "1"
  -- Otherwise we are very lazily just going to assume it's a text file. This
  -- will be improved in the future.
  | otherwise = restOfLink "0"
 where
  path :: FilePath
  path = T.unpack $ metaPath postMeta

  restOfLink :: String -> String
  restOfLink itemType = show $ MenuLink itemType (T.unpack $ metaTitle postMeta) path Nothing Nothing


-- TODO: Maybe belongs in common.
-- | Representation of a gophermap menu entry, specifically links and not info items.
data MenuLink = MenuLink
  { linkType :: String
  , displayString :: String
  , selector :: String
  , server :: Maybe String
  , port :: Maybe Int }

-- | Make a spacecookie spec .gophermap link.
--
-- gopherfiletypeNAME\t [SELECTOR [\tSERVER [\tPORT]]]
--
-- https://sternenseemann.github.io/spacecookie/spacecookie.gophermap.5.html
--
-- >>> :{
--  let
--    menuLink = MenuLink
--      { linkType="0"
--      , displayString="Apple Pie Recipe"
--      , selector="recipe.txt"
--      , server=Just "example.org"
--      , port=Just 70
--      }
--  in show menuLink
-- :}
-- "0Apple Pie Recipe\t/recipe.txt\texample.org\t70"
instance Show MenuLink where
  show ml =
    let firstPart =
          intercalate "\t"
            [ (linkType ml) ++ (displayString ml)
            , "/" ++ (selector ml) -- FIXME: join path?
            ]
        secondPart =
          fromMaybe "" (server ml >>= \x -> port ml >>= \y -> Just $ "\t" ++ x ++ "\t" ++ (show y))
    in firstPart ++ secondPart

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Time.Types
-- >>> :{
--  let
--    dateTime = DateTime (Date 1986 February 21) (TimeOfDay 0 0 0 0)
--    frontMatter =
--      FrontMatter 
--        (Just dateTime)
--        (Just dateTime)
--        (Just "Zelda is Released Today!")
--        (Just "A Video Game Fan")
--        (Just ["games", "nintendo"])
--        (Just "post")
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--    postMeta = fromJust . pairToPostMeta $ ("zelda-release.txt", Just frontMatter)
--    dateTime2 = DateTime (Date 1985 September 13) (TimeOfDay 0 0 0 0)
--    frontMatter2 =
--      FrontMatter 
--        (Just dateTime2)
--        (Just dateTime2)
--        (Just "Mario is Released Today!")
--        (Just "A Video Game Fan")
--        (Just ["games", "nintendo"])
--        (Just "post")
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--    postMeta2 = fromJust . pairToPostMeta $ ("mario-release.txt", Just frontMatter2)
--    dateTime3 = DateTime (Date 1971 November 3) (TimeOfDay 0 0 0 0)
--    frontMatter3 =
--      FrontMatter 
--        (Just dateTime3)
--        (Just dateTime3)
--        (Just "Unix is Released Today!")
--        (Just "Computer Nerd")
--        (Just ["computing"])
--        (Just "post")
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--    postMeta3 = fromJust . pairToPostMeta $ ("unix-release.txt", Just frontMatter3)
--    dateTime4 = DateTime (Date 2021 June 19) (TimeOfDay 20 36 0 0)
--    frontMatter4 =
--      FrontMatter 
--        (Just dateTime4)
--        (Just dateTime4)
--        (Just "Duplicate Post Title")
--        Nothing
--        Nothing
--        (Just "post")
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--    postMeta4 = fromJust . pairToPostMeta $ ("duplicate-post-title.txt", Just frontMatter4)
--    dateTime5 = DateTime (Date 2021 June 10) (TimeOfDay 06 25 0 0)
--    frontMatter5 =
--      FrontMatter 
--        (Just dateTime5)
--        (Just dateTime5)
--        (Just "Duplicate Post Title")
--        Nothing
--        Nothing
--        (Just "post")
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--        Nothing
--    postMeta5 = fromJust . pairToPostMeta $ ("duplicate-post-title2.txt", Just frontMatter5)
--    postMetas = [postMeta, postMeta2, postMeta3, postMeta4, postMeta5]
-- :}

{- $phlogIndexes

A phlog index is like blog indexes. There are different ways to index the posts to a phog.
There's the main index which simply is a list of *all* of the blog posts in order of date.
There's also individual tag indexes, which lists posts belonging to a specific tag. There's
also an index of all the tags, showing a few of the msot recent posts belonging to those
tags. The pattern here is that there's different ways of listing the meta/information
belonging to phlog posts, in a way that's presentable and easily perused by the user.

These specific kinds of indexes are the type that gets written to file to be browsed
in gopher. These same indexes also have associated RSS feeds.
-}
