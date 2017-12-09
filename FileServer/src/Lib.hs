-- This file is commented extensively for non-haskell programmers

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

module Lib
    ( startApp
    ) where

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
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import  System.Directory 
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
import           RestFileAPI
import           AESLib


startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do

  warnLog $ "Starting File Server."
  let settings = setPort (read fileServerPort) $ setLogger aplogger defaultSettings
  runSettings settings app

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
server = request_download_file
      :<|> request_upload_file
      :<|> request_transaction_ready_to_commit_response
      :<|> request_transaction_upload
      :<|> request_transaction_complete
    where
        -- user requires to download a file, send corresponding file to user
        request_download_file :: FileDownloadRequest -> Handler FileDownloadResponse
        request_download_file request = liftIO $ do

            warnLog $ "request to download file"

            -- extract field for essential information
            let encrypted_tmp_key = getDownloadPvtKey request
            let tmp_key = decryption encrypted_tmp_key myPvk

            let encrypted_file_name = getDownloadFileName request
            let file_name = decryption encrypted_file_name myPvk

            -- putStrLn "File Name: " ++ fn

            -- get file from file system
            path <- getCurrentDirectory
            file_data <- readFile (path ++ filePath ++ file_name)                         -- read the file
            let return_data = encryption file_data tmp_key                 -- encrypt with tmp kety
            return $ FileDownloadResponse return_data                      -- return the file

            where 

              getDownloadPvtKey ( FileDownloadRequest pvk _ ) = pvk
              getDownloadFileName ( FileDownloadRequest _ n ) = n

        -- user requires to upload files
        request_upload_file :: FileUploadRequest -> Handler Bool
        request_upload_file request = liftIO $ do
            warnLog $ "request to upload file"

            let encrypted_tmp_key = getUploadPvtKey request
            let tmp_key = decryption encrypted_tmp_key myPvk

            let file_name = getUploadFileName request                      

            let encrypted_file_data = getUploadFileData request
            let file_data = decryption encrypted_file_data encrypted_tmp_key

            putStrLn ( "File Name: " ++ ( file_name ) )
            putStrLn ( "File Data: " ++ ( file_data ) )
            
            -- store the file 
            path <- getCurrentDirectory
            putStrLn ( "Current Path: " ++ ( path ) )
            writeFile (path ++ filePath ++ file_name) file_data        -- write the file or replace the file
            return True  

            where 

              getUploadPvtKey ( FileUploadRequest pvk _ _) = pvk
              getUploadFileName ( FileUploadRequest _ n _) = n
              getUploadFileData ( FileUploadRequest _ _ d) = d

        request_transaction_ready_to_commit_response :: Handler Bool
        request_transaction_ready_to_commit_response = liftIO $ do
          -- send ready to commit response to transaction server so that transaction server knows that it is ready
          return True


        request_transaction_upload :: TransactionUploadRequestToFileServer -> Handler Bool
        request_transaction_upload request = liftIO $ do

          warnLog $ "request to shadow-upload a file" 

          let encrypted_tmp_key = getTransactionUploadPvtKey request
          let tmp_key = decryption encrypted_tmp_key myPvk

          let file_name = getTransactionUploadFileName request

          let encrypted_file_data = getTransactionUploadFileData request
          let file_data = decryption encrypted_file_data myPvk

          putStrLn ( "File Name: " ++ ( file_name ) )
          putStrLn ( "File Data: " ++ ( file_data ) )

          let tid = getTransactionUploadID request

          -- store the file in a temporary directory, it will be sent to the actual directory when transaction is requested to terminate 
          --( We call this shadowing)
          path <- getCurrentDirectory
          putStrLn ( "Current Path: " ++ ( path ) )
          writeFile (path ++ shadowFilePath ++ file_name) file_data                           

          -- keep the record of the shadow file
          let r = ShadowRecord tid file_name
          withMongoDbConnection $ upsert (select ["transaction_id" =: tid] "SHADOW_RECORD") $ toBSON r

          return True

          where 

            getTransactionUploadPvtKey ( TransactionUploadRequestToFileServer pvk _ _ _) = pvk
            getTransactionUploadFileName ( TransactionUploadRequestToFileServer _ n _ _) = n
            getTransactionUploadFileData ( TransactionUploadRequestToFileServer _ _ d _) = d
            getTransactionUploadID ( TransactionUploadRequestToFileServer _ _ _ id) = id

        request_transaction_complete :: String -> Handler Bool
        request_transaction_complete tid = liftIO $ do

          -- querry all the relevent shadow files record
          -- for all the relevent shadow file( e.g qurried file record)
          -- move them to the actual file directory
          
          sr <- findShadowRecord tid

          transaction_completion_process sr
          return True

          where                
            transaction_completion_process [] = putStrLn "transaction on that server completed"
            transaction_completion_process (x:xs) = do

              -- move all the relevent files in shadow file directory to the actual file directory
              let file_name = getShadowFileName x
              path <- getCurrentDirectory
              putStrLn ( "Current Path: " ++ ( path ) )
              fdata <- readFile (path ++ shadowFilePath ++ file_name)         -- read the file in shadow file directory
              writeFile (path ++ filePath ++ file_name) fdata                 -- write the create or edit(replace) the file in the actual directory

              transaction_completion_process xs

              where 
                getShadowFileName ( ShadowRecord _ fn ) = fn

            findShadowRecord tid = do
              withMongoDbConnection $ do
                docs <- find (select ["transaction_id" =: tid] "SHADOW_RECORD") >>= drainCursor  
                return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowRecord) docs


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





-- File Server will act as a "Client" to upload the replicated file to the assigned secondary server
-- | function to build the client environment for performing a servant client rest call
-- It uses the host name and port parameters if Just x, or else uses envrionment variables
-- This uses an applicative programming style that is very condensed, and easy to understand when you get used to it,
-- compared to the alternative sequence of calls and subsequent record construction.

env :: Maybe String -> Maybe String -> IO SC.ClientEnv
env host port = SC.ClientEnv <$> newManager defaultManagerSettings
                                               <*> (SC.BaseUrl <$> pure SC.Http
                                                               <*> (host <?> usehaskellHost)
                                                               <*> (read <$> (port <?> usehaskellPort))
                                                               <*> pure "")
 where
   (<?>) :: Maybe a -> IO a -> IO a
   h <?> f = case h of
     Just hst -> return hst
     Nothing  -> f

   -- | The url endpoint for contactingt the use-haskell service
   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True

   -- | The neo4j port
   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8080" True


   -- | Helper function to simplify the setting of environment variables
   -- function that looks up environment variable and returns the result of running funtion fn over it
   -- or if the environment variable does not exist, returns the value def. The function will optionally log a
   -- warning based on Boolean tag

   -- note that this is a good example of a commonly required function that could usefully be put in a shared library
   -- but I'm not going to do that for now.

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fn def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fn s
         Nothing -> warn' warn env def

    where warn' :: Show b => Bool -> String -> b -> IO b
          warn' wn e s =  do
            when wn $ putStrLn $ "Environment variable: " ++ e ++
                                    " is not set. Defaulting to " ++ (show s)
            return s

