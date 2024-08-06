{- | Copied from the spacecookie project, with some modifications, mostly
to avoid having to build it from scratch in deployment environments.

https://github.com/sternenseemann/spacecookie/

-}
{-# LANGUAGE OverloadedStrings #-}
module SpacecookieClone.Serve where

import SpacecookieClone.Config
import SpacecookieClone.FileType
import SpacecookieClone.Systemd

import Network.Gopher
import Network.Gopher.Util (sanitizePath, boolToMaybe, dropPrivileges)
import Network.Gopher.Util.Gophermap
import qualified Data.ByteString as B
import Control.Applicative ((<|>))
import Control.Exception (catches, Handler (..))
import Control.Monad (when, unless)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bifunctor (first)
import Data.ByteString.Builder (Builder ())
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Exit
import System.FilePath.Posix.ByteString ( RawFilePath, takeFileName, (</>)
                                        , dropDrive, decodeFilePath
                                        , encodeFilePath)
import qualified System.Log.FastLogger as FL
import System.Posix.Directory (changeWorkingDirectory)
import System.Socket (SocketException ())

runServer :: FilePath -> IO ()
runServer configFile = do
  doesFileExist configFile >>=
    (flip unless) (die "could not open config file")
  config' <- eitherDecodeFileStrict' configFile
  case config' of
    Left err -> die $ "failed to parse config: " ++ err
    Right config -> do
      changeWorkingDirectory (rootDirectory config)
      (logHandler, logStopAction) <- fromMaybe (Nothing, pure ())
        . fmap (first Just) <$> makeLogHandler (logConfig config)
      let cfg = GopherConfig
            { cServerName = serverName config
            , cListenAddr = listenAddr config
            , cServerPort = serverPort config
            , cLogHandler = logHandler
            }
          logIO = fromMaybe noLog logHandler

      let setupFailureHandler e = do
            logIO GopherLogLevelError
              $  "Exception occurred in setup step: "
              <> toGopherLogStr (show e)
            logStopAction
            exitFailure
          catchSetupFailure a = a `catches`
            [ Handler (setupFailureHandler :: SystemdException -> IO ())
            , Handler (setupFailureHandler :: SocketException -> IO ())
            ]

      catchSetupFailure $ runGopherManual
        (systemdSocket cfg)
        (afterSocketSetup logIO config)
        (\s -> do
          _ <- notifyStopping
          logStopAction
          systemdStoreOrClose s)
        cfg
        (spacecookie logIO)

afterSocketSetup :: GopherLogHandler -> Config -> IO ()
afterSocketSetup logIO cfg = do
  case runUserName cfg of
    Nothing -> pure ()
    Just u  -> do
      dropPrivileges u
      logIO GopherLogLevelInfo $ "Changed to user " <> toGopherLogStr u
  _ <- notifyReady
  pure ()

makeLogHandler :: LogConfig -> IO (Maybe (GopherLogHandler, IO ()))
makeLogHandler lc =
  let wrapTimedLogger :: FL.TimedFastLogger -> FL.FastLogger
      wrapTimedLogger logger str = logger $ (\t ->
        "[" <> FL.toLogStr t <> "]" <> str)
      formatLevel lvl =
        case lvl of
          GopherLogLevelInfo  -> "[info] "
          GopherLogLevelWarn  -> "[warn] "
          GopherLogLevelError -> "[err ] "
      processMsg =
        if logHideIps lc
          then hideSensitive
          else id
      logHandler :: FL.FastLogger -> GopherLogLevel -> GopherLogStr -> IO ()
      logHandler logger lvl msg = when (lvl <= logLevel lc) . logger
        $  formatLevel lvl
        <> ((FL.toLogStr :: Builder -> FL.LogStr) . fromGopherLogStr . processMsg $ msg)
        <> "\n"
      logType = FL.LogStderr FL.defaultBufSize
   in sequenceA . boolToMaybe (logEnable lc) $ do
     (logger, cleanup) <-
       if logHideTime lc
         then FL.newFastLogger logType
         else first wrapTimedLogger <$> do
           timeCache <- FL.newTimeCache FL.simpleTimeFormat
           FL.newTimedFastLogger timeCache logType
     pure (logHandler logger, cleanup)

noLog :: GopherLogHandler
noLog = const . const $ pure ()

spacecookie :: GopherLogHandler -> GopherRequest -> IO GopherResponse
spacecookie logger req = do
  let selector = requestSelector req
      path = "." </> dropDrive (sanitizePath selector)
  pt <- gopherFileType path

  case pt of
    Left PathIsNotAllowed ->
      pure . ErrorResponse $ mconcat
        [ "Accessing '",  selector, "' is not allowed." ]
    Left PathDoesNotExist -> pure $
      if "URL:" `B.isPrefixOf` selector
        then ErrorResponse $ mconcat
          [ "spacecookie does not support proxying HTTP, "
          , "try using a gopher client that supports URL: selectors. "
          , "If you tried to request a resource called '"
          , selector, "', it does not exist." ]
        else ErrorResponse $ mconcat
          [ "The requested resource '", selector
          , "' does not exist or is not available." ]
    Right ft ->
      case ft of
        Error -> pure $ ErrorResponse $ "An unknown error occurred"
        -- always use gophermapResponse which falls back
        -- to directoryResponse if there is no gophermap file
        Directory -> gophermapResponse logger path
        _ -> fileResponse logger path

fileResponse :: GopherLogHandler -> RawFilePath -> IO GopherResponse
fileResponse _ path = FileResponse <$> B.readFile (decodeFilePath path)

makeAbsolute :: RawFilePath -> RawFilePath
makeAbsolute x = fromMaybe x
  $   boolToMaybe ("./" `B.isPrefixOf` x) (B.tail x)
  <|> boolToMaybe ("." == x) "/"

directoryResponse :: GopherLogHandler -> RawFilePath -> IO GopherResponse
directoryResponse _ path =
  let makeItem :: Either a GopherFileType -> RawFilePath -> Either a GopherMenuItem
      makeItem t file = do
        fileType <- t
        pure $
          Item fileType (takeFileName file) file Nothing Nothing
   in do
     dir <- map ((path </>) . encodeFilePath)
       <$> getDirectoryContents (decodeFilePath path)
     fileTypes <- mapM gopherFileType dir

     pure . MenuResponse . rights
       $ zipWith makeItem fileTypes (map makeAbsolute dir)

gophermapResponse :: GopherLogHandler -> RawFilePath -> IO GopherResponse
gophermapResponse logger path = do
  let gophermap = path </> ".gophermap"
      gophermapWide = decodeFilePath gophermap
  exists <- doesFileExist gophermapWide
  parsed <-
    if exists
      then parseOnly parseGophermap <$> B.readFile gophermapWide
      else pure $ Left "Gophermap file does not exist"
  case parsed of
    Left err -> do
      when exists . logger GopherLogLevelWarn
        $  "Could not parse gophermap at " <> toGopherLogStr gophermap
        <> ": " <> toGopherLogStr err
      directoryResponse logger path
    Right right -> pure
      $ gophermapToDirectoryResponse (makeAbsolute path) right
