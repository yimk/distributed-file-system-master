
-- | These are language extensions. Haskell has a great many language
-- extensions but in practice you do not need to knwo much about them. If you
-- use a library that needs them, then the library documentation will tell you which
-- extensions you neeed to include. If you try to write code that needs particular extensions,
-- then the haskell compiler is smart enough typically to be able to suggest which extensions
-- you should switch on by including an entry here.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Haskell code is structured as sets of functions that sit within Modules. The basic rule is that a module with a
-- particular name (for example Lib) sits within a .hs file of the same name (eg. Lib.hs). The module statement is of
-- the form `module MODULE_NAME (EXPORTED_FUNCTIONS) where`. Everything following this is part of the module. There are
-- no brackets or any other syntax to worry about.
module Lib
    ( startApp
    ) where

-- | Imports work like most other languages and are essentially library includes. The functions of the lirbary become
-- immediately accessible in the code of the module. There are various ways in which imports can be modified. For
-- example, one may `import qualified X as Y` which imports a library in such a way that the functions of the library
-- must be prefixed with `Y.`. One can always prefix a libraries functions with the import string, when calling them.
-- You will occasionally have reason to import libraries that have common function names by coincidence. You can use
-- qualified imports of full prefixes to disambiguate. The compiler will tell you where the problem is if this occurs.

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import qualified Data.List.Split              as DLS
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
-- import           UseHaskellAPI
import           RestDirectoryAPI




startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  -- warnLog "Starting Directory Server."


  forkIO $ taskScheduler 5

  let settings = setPort ( read directoryServerPort ) $ setLogger aplogger defaultSettings
  runSettings settings app

-- this is the original startApp that stack new servant builds
--startApp :: IO ()
--startApp = run 8080 app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  -- warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getUploadDirectory
      :<|> getDownloadDirectory
     
    where 
       
        getUploadDirectory :: String -> Handler [DirectoryResponse]
        getUploadDirectory file_name = liftIO $ do

          warnLog $ "A client is requesting directory to upload file: " ++ file_name

          -- user either update existing file or create a new file
          -- we find out that by checking if the file name exists
          -- if update, current directories and file assigned name will used        (client may cache this value)
          -- if create, new directories and file assigned name will be given

          querried_list <- findDirectoryList file_name                  -- querry rows with file_name
          let e = isFileExisted querried_list                           -- check if it's empty( file exist? )

          case e of

            
            True -> do
              putStrLn "File record exists, return old directory"
              return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DirectoryResponse) querried_list   -- return directories querried directory list
            False -> do

              
              -- create the file
              putStrLn "File not existed, create new directory"

              -- give an assigned name to that file, the file uses this name in the file server
              -- this is for scale
              -- assigned current FILE_URL_RECORD size (which is the number of existing file )       
              l <- getCurrentFURLength
              let file_type = last (DLS.splitOn "." file_name)                                 -- get the type of the file e.g 'txt'
              let assigned_file_name = (show l) ++ "." ++ file_type
              let prt = "Assigned Name: " ++ (assigned_file_name)
              putStrLn prt


              -- I am assuming there are multiple file servers
              -- file server 1 is the primary server
              -- however, this is not important 
              -- as when the client is uploading a file
              -- this file will be braodcast to all the relevent file server(in here it is all the existing file server)
              -- when client downloads file, he can choose to donwload it from primary server or from secondary servers(replication)

              return_directory_response serverList [] 0 file_name assigned_file_name 
              
              where
                -- assuming there is at least one file server
                return_directory_response [] ds _ _ _= do
                  putStrLn "directories returned"
                  return ds

                return_directory_response (x:xs) ds rn file_name assigned_file_name = do

                  let FileServer _ h p = x

                  -- insert record into database
                  let d = DirectoryResponse file_name assigned_file_name h p $ show rn
                  withMongoDbConnection $ upsert (select ["file_name" =: file_name] "FILE_URL_RECORD") $ toBSON d

                  return_directory_response xs (d:ds) (rn+1) file_name assigned_file_name

 
        getDownloadDirectory :: String -> Handler [DirectoryResponse]
        getDownloadDirectory file_name = liftIO $ do
            
            putStrLn ( "requesting directory to download file: " ++ file_name  )  
            
            -- return list of record, one of them is primary record, others are replicated record.
            querried_list <- findDirectoryList file_name                  -- querry rows with file_name
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DirectoryResponse) querried_list   -- return directories querried directory list


isFileExisted [] = False
isFileExisted _ = True

findDirectoryList n = do 

  withMongoDbConnection $ do
    docs <- find (select ["file_name" =: n] "FILE_URL_RECORD") >>= drainCursor  
    return docs

getCurrentFURLength = do
  withMongoDbConnection $ do
    docs <- find (select [] "FILE_URL_RECORD") >>= drainCursor
    return (length docs) 


-- What follows next is some helper function code that makes it easier to do warious things such as use
-- a mongoDB, post console log statements define environment variables for use in your programmes and so forth.
-- The code is not written particularly to be understood by novice Haskellers, but should be useable relatively easily
-- as set out above.

-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  let ip = "127.0.0.1"
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def


