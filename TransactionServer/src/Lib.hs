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
import           RestTransactionAPI
import           Data.IORef
import           AESLib

-- Design

-- start a transaction
  -- client request transaction server to start a transaction 
  -- a transaction id is given to that client

-- sequence of action
  -- client will use it to send a sequence of request             e.g upload fname fdata tid
  -- everytime when client request an action
  -- transaction server send it to the relevent file server
  -- however, the targeted file will not be created/edited -> it is not uploaded to the normal directory
  -- instead a shadow copy of that file will be created within that file server's shadow directory

-- terminate the transaction
  -- client terminates the transaction by sending an end-transaction request to the transaction server
  -- transaction server stores the transaction id in a log file( in here i am using TransactionLog DB instead of it)

  -- transaction server loads the logfile every few second ( task schedular)
  -- if log file is not empty

    -- the transaction server then send a ready to commit request to all file server that is relevent to this trasaction id
    -- if all response true
    -- request all of them to move the shadow copy to the actual file storage directory( create/edit is performed )


startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting Directory Server."

  forkIO $ taskScheduler 5

  let settings = setPort ( read transactionServerPort ) $ setLogger aplogger defaultSettings
  runSettings settings app

-- this is the original startApp that stack new servant builds
--startApp :: IO ()
--startApp = run 8080 app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  -- warnLog $ "Task scheduler operating."

  -- check if there is complete transaction record, if there is, procedure it
  processLog

  -- wait for a while
  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

  where 
    -- return list of record, one of them is primary record, others are replicated record.             
    processLog = do

      log <- getLog  
      case log of

        [] -> putStrLn "No Completed Transaction"

        (x:xs) -> do

          -- find out the id of the complete transaction
          let tid = getTransactionLogID x 
          slist <- getTransactionRecord tid

          -- find out if all file servers are ready-to-commit
          is_ready <- check_if_fserver_ready_to_commit slist tid
          
          case is_ready of
            True -> do 
              putStrLn "All File Servers are ready to commit"
              tellReleventFileServerToTerminate slist tid

            False -> putStrLn "Some servers aren't ready to commit"



getTransactionLogID (TransactionLog x) = x         

tellReleventFileServerToTerminate [] tid = putStrLn "All the relevent file servers terminated the transaction"
tellReleventFileServerToTerminate (x:xs) tid = do

  let h = getTransactionRecordHost x
  let p = getTransactionRecordPort x

  manager <- newManager defaultManagerSettings
  response <- SC.runClientM (transactionCompleteRequest tid) (SC.ClientEnv manager (SC.BaseUrl SC.Http h (read p) ""))

  -- if current file server not ready( e.g no response)
  case response of
    Left err -> do
      -- shouldn't happen as the server just asked if all the servers are ready
      putStrLn $ "error: " ++ show err

    Right r -> do
      putStrLn $ "File server " ++ h ++ "/" ++ p ++ "has temrinated the transaction"
      -- go to next server
      tellReleventFileServerToTerminate xs tid
          
check_if_fserver_ready_to_commit [] tid = return True
check_if_fserver_ready_to_commit (x:xs) tid= do

  let h = getTransactionRecordHost x
  let p = getTransactionRecordPort x

  -- check if all the file server are ready to commit
  manager <- newManager defaultManagerSettings
  response <- SC.runClientM (readyToCommitRequest) (SC.ClientEnv manager (SC.BaseUrl SC.Http h (read p) ""))

  -- if current file server not ready( e.g no response)
  case response of
    Left err -> do
      putStrLn $ "error: " ++ show err
      putStrLn $ "File server " ++ h ++ "/" ++ p ++ "doesn't seems to be ready"
      return False

    Right r -> do
      putStrLn $ "File server " ++ h ++ "/" ++ p ++ "is ready"
      -- go to next server
      check_if_fserver_ready_to_commit xs tid

-- resendCompleteTransactionRequestToFileServer h p tid = do

--   manager <- newManager defaultManagerSettings
--   response <- SC.runClientM (transactionCompleteRequest tid) (SC.ClientEnv manager (SC.BaseUrl SC.Http h (read p) ""))

--   case response of
--     Left err -> do
--       putStrLn $ "error: " ++ show err
--       putStrLn $ "File server " ++ h ++ "/" ++ p ++ "seems to be disconnected"
--       putStrLn "System will try again in 5 second"

--       threadDelay $ 5 * 1000000
--       resendCompleteTransactionRequestToFileServer h p tid

--     Right r -> do
--       putStrLn $ "File server " ++ h ++ "/" ++ p ++ "is ready"
--       -- return back to the transaction
                
              
            

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = startTransactionRequest
      :<|> endTransactionRequest
      :<|> transactionUpload
     
    where 
        startTransactionRequest :: String -> Handler String
        startTransactionRequest _ = liftIO $ do
          -- client id is required, however it is not used
          -- assign a transaction id to the user

          -- Transaction id equal to the length of transaction record
          withMongoDbConnection $ do
            -- get the entire table
            docs <- find (select [] "TRANSACTION_RECORD") >>= drainCursor 

            let a = length docs
            -- putStrLn $ "A client is requesting a transaction process/nTransaction ID:" ++ (show a) 
            return $ show a 


        endTransactionRequest :: String -> Handler Bool
        endTransactionRequest tid = liftIO $ do
            
          putStrLn $ "Request to terminates transaction: " ++ (read tid)      
          
          let complete_record = TransactionLog tid

          -- write it into log file
          withMongoDbConnection $ upsert (select [] "TRANSACTION_LOG") $ toBSON complete_record
          return True

        transactionUpload :: TransactionUploadRequestFromClient -> Handler Bool
        transactionUpload request = liftIO $ do
          
          -- extract field
          let fname = getTransactionFileName request
          let file_data = getTransactionFileData request
          let tid = getTransactionFileID request

          -- get directories
          manager <- newManager defaultManagerSettings
          file_dircetories <- SC.runClientM (getUploadDirectory fname) (SC.ClientEnv manager (SC.BaseUrl SC.Http directoryServerHost (read directoryServerPort) ""))

          case file_dircetories of

            Left err -> do
              putStrLn $ "error: " ++ show err
              return False

            Right directories -> do

              putStrLn "Receive Directory From Directory Server."

              -- handle each directory iteratively
              transactionUploadFileToDirectories directories tid

              -- return response
              
              where 

                transactionUploadFileToDirectories [] _ = do
                  putStrLn "Transaction Uploading is completed"
                  return True

                transactionUploadFileToDirectories (directory:xs) tid = do 
              
                  -- extract fields
                  let target_file_assigned_name = getAssginedFileNameFromDR directory
                  let host_of_file_server = getHostFromDR directory
                  let port_of_file_server = getPortFromDR directory
                  let id_of_file_server = getSIDFromDR directory

                  putStrLn ( "Target File Assigned Name: " ++ ( show target_file_assigned_name ) )
                  putStrLn ( "Target Server ID: " ++ ( show id_of_file_server ) )
                  putStrLn ( "Target Host: " ++ ( show host_of_file_server ) )
                  putStrLn ( "Target Port: " ++ ( show port_of_file_server ) )
                  
                
                  -- get ticket for uploading permission
                  putStrLn "Requesting Permission from Authentication Server"

                  let id_of_file_server = encryption id_of_file_server myPvk                            -- encrypt target server id
                  let target_file_assigned_name = encryption target_file_assigned_name myPvk            -- encrypt target file name
                  let tr = TicketRequest clientID id_of_file_server target_file_assigned_name

                  ticket_resp <- SC.runClientM (requestTicket tr) (SC.ClientEnv manager (SC.BaseUrl SC.Http authenticationServerHost (read authenticationServerPort) ""))

                  case ticket_resp of
                    Left err -> do
                      putStrLn $ "error: " ++ show err
                      return False

                    Right ticket -> do

                      -- upload the file to file server's shadow directory
                      let tmp_pbk = getPBKFromTR ticket
                      let tmp_pvk = getPVKFromTR ticket
                      let encrypted_file_data = encryption file_data tmp_pbk
                      let fn = getEncryptedFileNameFromTR ticket

                      let ur = TransactionUploadRequestToFileServer tmp_pvk fn encrypted_file_data tid
                      upload_resp <- SC.runClientM (shadowUploadRequest ur) (SC.ClientEnv manager (SC.BaseUrl SC.Http host_of_file_server (read port_of_file_server) ""))

                      case upload_resp of
                        Left err -> do
                          putStrLn $ "error: " ++ show err
                          return False

                        Right _ -> do
                          -- store the record 
                          store_trans_record host_of_file_server port_of_file_server id_of_file_server tid

                          -- send file to next file server's shadow directory
                          transactionUploadFileToDirectories xs tid

                      
            



-- some function 

store_trans_record h p sid tid = do

  docs <- getTransactionRecord tid
  case docs of
    [] -> do
      -- if server is not part of the transaction yet
      let r = TransactionRecord tid sid h p
      withMongoDbConnection $ upsert (select ["transaction_id" =: tid] "TRANSACTION_RECORD") $ toBSON r
      -- withMongoDbConnection $ upsert (select ["transaction_id" =: tid] "TRANSACTION_RECORD") $ toBSON r
      return True

    xs -> do
      -- putStrLn "No need to record as server is already part of the transaction"
      return True

getAssginedFileNameFromDR ( DirectoryResponse _ afn _ _ _) = afn 
getHostFromDR ( DirectoryResponse _ _ ash _ _) = ash
getPortFromDR ( DirectoryResponse _ _ _ asp _) = asp
getSIDFromDR ( DirectoryResponse _ _ _ _ asid ) = asid

getPBKFromTR ( TicketResponse b _ _) = b     
getPVKFromTR ( TicketResponse _ v _) = v
getEncryptedFileNameFromTR ( TicketResponse _ _ fn) = fn

getTransactionRecord tid = do
  withMongoDbConnection $ do
    docs <- find (select ["transaction_id" =: tid] "TRANSACTION_RECORD") >>= drainCursor  
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionRecord) docs

getLog = do
  withMongoDbConnection $ do
    docs <- find (select [] "TRANSACTION_LOG") >>= drainCursor  
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransactionLog) docs

getTransactionRecordHost (TransactionRecord _ _ h _) = h
getTransactionRecordPort (TransactionRecord _ _ _ p) = p                          

getTransactionFileName ( TransactionUploadRequestFromClient fn _ _ ) = fn
getTransactionFileData ( TransactionUploadRequestFromClient _ d _ ) = d
getTransactionFileID ( TransactionUploadRequestFromClient _ _ id ) = id

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


