{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
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
    ( someFunc
    ) where

import           Control.Monad                      (join, when)
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.List                          as DL
import           Data.List.Split
-- import           Data.Cache
import           Distribution.PackageDescription.TH

import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Options.Applicative
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           System.Console.ANSI
import           System.Environment
import           UseHaskellAPI
import           UseHaskellAPIClient
import           RestClientAPI
import           AESLib
import           Constant
import           Database.MongoDB                   as DB

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
-- | helper functions to change color in ansi terminal output (mor for the fun of it)
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]


-- | output a command line banner
banner :: IO ()
banner = do
  progName <- getProgName
  putStrLn "Start Program"



-- | Command line option handlers, one for each command
-- These are called from the options parsing and do the actuall work of the program.
  
doDownloadFile :: String -> Maybe String -> Maybe String -> IO ()
doDownloadFile fname host port = do

  putStrLn "Download a File"

  
  directories <- getDirectoryForDownload fname

  -- print the server list of the file
  putStrLn "Please select one of the following server( by entering the server id"   
  print_server_list directories

  -- ask user to select one of the server
  rn <- getLine  
  let directory = directories !! (read rn)

  -- extract fields
  let target_file_assigned_name = getAssginedFileNameFromDR directory
  let host_of_file_server = getHostFromDR directory
  let port_of_file_server = getPortFromDR directory
  let id_of_file_server = getSIDFromDR directory

  putStrLn ( "Target File Assigned Name: " ++ ( show target_file_assigned_name ) )
  putStrLn ( "Target Server ID: " ++ ( show id_of_file_server ) )
  putStrLn ( "Target Host: " ++ ( show host_of_file_server ) )
  putStrLn ( "Target Port: " ++ ( show port_of_file_server ) )
  
  -- check if the file is locked

  let file_info = FileInformation ( show id_of_file_server ) ( show target_file_assigned_name )

  manager <- newManager defaultManagerSettings
  is_locked <- SC.runClientM (isLocked file_info) (SC.ClientEnv manager (SC.BaseUrl SC.Http lockServerHost (read lockServerPort) ""))

  case is_locked of
    Left err -> putStrLn $ "error: " ++ show err
    Right lk -> do
      
      case lk of
        True -> putStrLn "Sorry, the file is locked"
        False -> do 
          putStrLn "The file is accessible"
          

          -- get ticket for downloading permission
          putStrLn "Requesting Permission from Authentication Server"

          let encrypted_id_of_file_server = encryption id_of_file_server myPvk                            -- encrypt target server id
          let encrypted_target_file_assigned_name = encryption target_file_assigned_name myPvk            -- encrypt target file name
          let tr = TicketRequest clientID encrypted_id_of_file_server encrypted_target_file_assigned_name


          manager <- newManager defaultManagerSettings
          ticket <- SC.runClientM (requestTicket tr) (SC.ClientEnv manager (SC.BaseUrl SC.Http authenticationServerHost (read authenticationServerPort) ""))


          case ticket of
            Left err -> putStrLn $ "error: " ++ show err
            Right tk -> do

              putStrLn "Receive Ticket From Authentication Server"

              -- extract fields
              let tmp = getPBKFromTR tk                            -- decryptable
              let pbk_for_me = decryption tmp myPvk                -- decrypt it

              let pvk_for_target = getPVKFromTR tk                        -- undecryptable
              let fn_for_target = getEncryptedFileName tk                 -- undecryptable

              -- putStrLn "Start to download the file " ++ ( fname )

              let download_request = FileDownloadRequest pvk_for_target fn_for_target
              file_download_response <- SC.runClientM (downloadFile download_request) (SC.ClientEnv manager (SC.BaseUrl SC.Http host_of_file_server (read port_of_file_server) ""))
  
              case file_download_response of
                Left err -> putStrLn $ "error: " ++ show err
                Right file_download_response -> do
                  
                  putStrLn "Download Complete"

                  let d = getDownloadFileData file_download_response
                  putStrLn "File Data:" 
                  putStrLn d 

      putStrLn "Task Complelte------------------------"

  where 
    print_server_list [] = putStrLn "\n"
    print_server_list (d:ds) = do
      let (DirectoryResponse _ _ _ _ id) = d
      putStrLn "ID: d"
      print_server_list ds


doUploadFile :: String -> String -> Maybe String -> Maybe String-> IO ()
doUploadFile fname fpath host port =  do
  
  putStrLn "Upload a File"

  -- read the file
  file_data <- readFile (fpath)            
  
  directories <- getDirectoryForUploading fname 

  -- handle each directory iteratively
  uploadFileToDirectories directories file_data


  where 
    uploadFileToDirectories [] _ = putStrLn "Uploading is completed"
    uploadFileToDirectories (directory:xs) file_data = do 
      
      -- extract fields
      let target_file_assigned_name = getAssginedFileNameFromDR directory
      let host_of_file_server = getHostFromDR directory
      let port_of_file_server = getPortFromDR directory
      let id_of_file_server = getSIDFromDR directory

      putStrLn ( "Target File Assigned Name: " ++ ( show target_file_assigned_name ) )
      putStrLn ( "Target Server ID: " ++ ( show id_of_file_server ) )
      putStrLn ( "Target Host: " ++ ( show host_of_file_server ) )
      putStrLn ( "Target Port: " ++ ( show port_of_file_server ) )
      
      -- check if the file is locked
      let file_info = FileInformation ( show id_of_file_server ) ( show target_file_assigned_name )
      manager <- newManager defaultManagerSettings
      is_locked <- SC.runClientM (isLocked file_info) (SC.ClientEnv manager (SC.BaseUrl SC.Http lockServerHost (read lockServerPort) ""))

      case is_locked of
        Left err -> putStrLn $ "error: " ++ show err
        Right lk -> do
          case lk of 
            True -> putStrLn "File is Locked, Uploading is failed"

            False -> do 
              -- get ticket for uploading permission
              putStrLn "Requesting Permission from Authentication Server"

              let encrypted_id_of_file_server = encryption id_of_file_server myPvk                            -- encrypt target server id
              let encrypted_target_file_assigned_name = encryption target_file_assigned_name myPvk            -- encrypt target file name
              let tr = TicketRequest clientID encrypted_id_of_file_server encrypted_target_file_assigned_name

              manager <- newManager defaultManagerSettings
              putStrLn "Requesting Permission from Authentication Server"
              ticket <- SC.runClientM (requestTicket tr) (SC.ClientEnv manager (SC.BaseUrl SC.Http authenticationServerHost (read authenticationServerPort) ""))

              case ticket of
                Left err -> putStrLn $ "error: " ++ show err
                Right tk -> do

                  putStrLn "Receive Ticket From Authentication Server"

                  -- extract fields
                  let tmp = getPBKFromTR tk                            -- decryptable
                  let pbk_for_me = decryption tmp myPvk                 -- decrypt it

                  let pvk_for_target = getPVKFromTR tk                        -- undecryptable
                  let fn_for_target = getEncryptedFileName tk                 -- undecryptable

                  putStrLn ( "PBK For me: " ++ ( pbk_for_me ) )

                  -- putStrLn "Start to download the file " ++ ( fname )
                  let encrypted_file_data = encryption file_data myPvk
                  let upload_request = FileUploadRequest pvk_for_target fname encrypted_file_data

                  uploading_response <- SC.runClientM (uploadFile upload_request) (SC.ClientEnv manager (SC.BaseUrl SC.Http host_of_file_server (read port_of_file_server) ""))


                  case uploading_response of
                    Left err -> putStrLn $ "error: " ++ show err
                    Right uploading_response -> do
                      
                      case uploading_response of
                        True -> putStrLn "Uploading is Sucessful"
                        False -> putStrLn "Uploading is failed, Please try again later"

      -- upload file to the next directory
      putStrLn ( "\n" )
      uploadFileToDirectories xs file_data


doTransaction :: Maybe String -> Maybe String-> IO ()
doTransaction host port =  do
  
  putStrLn "Transaction start"
  putStrLn "Requesting transaction id from trasaction server"

  manager <- newManager defaultManagerSettings
  transaction_id <- SC.runClientM (startTransactionRequest clientID) (SC.ClientEnv manager (SC.BaseUrl SC.Http transactionServerHost (read transactionServerHost) ""))
  
  case transaction_id of
    Left err -> putStrLn $ "error: " ++ show err
    Right tid -> do

      getTransactionCommandFromClient tid

      where
        getTransactionCommandFromClient tid = do

          putStrLn "Please type one of the following command(make sure every word is seperated by one space)"
          putStrLn "create-or-edit <<file name>> <<file path>>"       
          putStrLn "close [NB:terminate the transaction]"


          input <- getLine
          let inputs = (DLS.splitOn " " input)
          let task = inputs !! 0

          putStrLn "close [NB:terminate the transaction]"

          case task of 

            "create-or-edit" -> do

              let fname = inputs !! 1
              let path = inputs !! 2
              fdata <- readFile ( path )
              let request = TransactionUploadRequestFromClient fname fdata tid
              
              is_locked <- SC.runClientM (transactionUpload request) (SC.ClientEnv manager (SC.BaseUrl SC.Http lockServerHost (read lockServerPort) ""))

              case is_locked of
                Left err -> putStrLn $ "error: " ++ show err
                Right l -> do 
                  case l of
                    True -> putStrLn "File is locked"
                    False -> do 
                      response <- SC.runClientM (transactionUpload request) (SC.ClientEnv manager (SC.BaseUrl SC.Http transactionServerHost (read transactionServerPort) ""))

                      case response of
                        Left err -> putStrLn $ "error: " ++ show err
                        Right r -> do
                          case r of 
                            False -> putStrLn "Request Failed, Please try again"
                            True -> putStrLn "Request Sucess"

              -- get client's next command
              getTransactionCommandFromClient tid

            "close" -> do
              -- essentially request the transaction server to end the transaction
              -- 
              response <- SC.runClientM (endTransactionRequest tid) (SC.ClientEnv manager (SC.BaseUrl SC.Http transactionServerHost (read transactionServerPort) ""))
              case response of
                Left err -> putStrLn $ "error: " ++ show err
                Right r -> do
                  case r of 
                    False -> putStrLn "Request Failed, Please try again"
                    True -> putStrLn "Request Sucess, transaction completed"

doLock :: Maybe String -> Maybe String -> String -> IO ()
doLock host port fname=  do

  -- get the directories from directory server
  directories <- getDirectoryForDownload fname
  let directory = directories !! 1

  -- extract fields
  let target_file_assigned_name = getAssginedFileNameFromDR directory
  let host_of_file_server = getHostFromDR directory
  let port_of_file_server = getPortFromDR directory
  let id_of_file_server = getSIDFromDR directory
  
  manager <- newManager defaultManagerSettings
  let file_info = FileInformation ( show id_of_file_server ) ( show target_file_assigned_name )
  lk_response <- SC.runClientM (enableLock file_info) (SC.ClientEnv manager (SC.BaseUrl SC.Http lockServerHost (read lockServerPort) ""))
  case lk_response of
    Left err -> putStrLn $ "error: " ++ show err
    Right lk -> putStrLn $ "File is Locked"

doUnLock :: Maybe String -> Maybe String -> String -> IO ()
doUnLock host port fname =  do

  -- get the directories from directory server
  directories <- getDirectoryForDownload fname
  let directory = directories !! 1

  -- extract fields
  let target_file_assigned_name = getAssginedFileNameFromDR directory
  let host_of_file_server = getHostFromDR directory
  let port_of_file_server = getPortFromDR directory
  let id_of_file_server = getSIDFromDR directory
  
  manager <- newManager defaultManagerSettings
  let file_info = FileInformation ( show id_of_file_server ) ( show target_file_assigned_name )
  lk_response <- SC.runClientM (disableLock file_info) (SC.ClientEnv manager (SC.BaseUrl SC.Http lockServerHost (read lockServerPort) ""))
  case lk_response of
    Left err -> putStrLn $ "error: " ++ show err
    Right lk -> putStrLn $ "File is UnLocked"



----------------------------------method-------------------------------------------------
            
getAssginedFileNameFromDR ( DirectoryResponse _ afn _ _ _) = afn 
getHostFromDR ( DirectoryResponse _ _ ash _ _) = ash
getPortFromDR ( DirectoryResponse _ _ _ asp _) = asp
getSIDFromDR ( DirectoryResponse _ _ _ _ asid ) = asid

getPBKFromTR ( TicketResponse b _ _) = b     
getPVKFromTR ( TicketResponse _ v _) = v
getEncryptedFileName ( TicketResponse _ _ fn) = fn

getDownloadFileData ( FileDownloadResponse d ) = d

getDirectoryForUploading fname = do

  -- check if this file's directory is cached
  dr <- getDirectoryFromCache fname

  case dr of 

    [] -> do 
      -- if empty(no history)
      let s = "Directory of file " ++ fname ++ " is not cached"
      putStrLn s

      -- get the directory from directory server
      manager <- newManager defaultManagerSettings

      dr <- SC.runClientM (getUploadDirectory fname) (SC.ClientEnv manager (SC.BaseUrl SC.Http directoryServerHost (read directoryServerPort) ""))
      putStrLn "Receive Upload Directory from directory server"

      case dr of 

        Left err -> do
          putStrLn $ "error: " ++ show err
          -- an empty directory is returned (no uploading as it is a iterative process)
          return ([] :: [DirectoryResponse])

        Right d -> do
          putStrLn "Receive Directory From Directory Server."
          -- cache it
          insertDirectoryToCache fname d
          -- return it
          return d

    d -> do
      let s = "Directory of file " ++ fname ++ " is cached\nCached Directory is used"
      putStrLn s
      return d

getDirectoryForDownload fname = do

  -- check if this file's directory is cached
  dr <- getDirectoryFromCache fname

  case dr of 

    [] -> do 
      -- if empty(no history)
      let s = "Directory of file " ++ fname ++ " is not cached"
      putStrLn s

      -- get the directory from directory server
      manager <- newManager defaultManagerSettings
       
      dr <- SC.runClientM (getDownloadDirectory fname) (SC.ClientEnv manager (SC.BaseUrl SC.Http directoryServerHost (read directoryServerPort) ""))
      putStrLn "Receive Download Directory from directory server"

      case dr of 

        Left err -> do
          putStrLn $ "error: " ++ show err
          -- an empty directory is returned (no uploading as it is a iterative process)
          return ([] :: [DirectoryResponse])

        Right d -> do
          putStrLn "Receive Directory From Directory Server."
          -- cache it
          insertDirectoryToCache fname d
          -- return it
          return d

    d -> do
      let s1 = "Directory of file " ++ fname ++ " is cached\nCached Directory is used"
      let s2 = "There are " ++ ( show (length d)) ++ "directories" 
      putStrLn s1
      putStrLn s2
      return d


getDirectoryFromCache fname = do
  withMongoDbConnection $ do
    docs <- DB.find (select ["file_name" =: fname] "DIRECTORY_CACHE ") >>= drainCursor  
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DirectoryResponse) docs

insertDirectoryToCache fname [] = do
  let p = "Directory for file '" ++ fname ++ "' is now cached"
  putStrLn p
insertDirectoryToCache fname (d:ds) = do
  withMongoDbConnection $ upsert (select ["file_name" =: fname] "DIRECTORY_CACHE") $ toBSON d
  insertDirectoryToCache fname ds



----------------------------------method-------------------------------------------------

-- | The options handling

-- First we invoke the options on the entry point.
someFunc :: IO ()
someFunc = do
  banner
  join $ execParser =<< opts

-- | Defined in the applicative style, opts provides a declaration of the entire command line
--   parser structure. To add a new command just follow the example of the existing commands. A
--   new 'doCall' function should be defined for your new command line option, with a type matching the
--   ordering of the application of arguments in the <$> arg1 <*> arg2 .. <*> argN style below.
-- opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName

  return $ info (   helper
                <*> subparser
                       (  command "upload-file"
                                  (withInfo ( doUploadFile
                                          <$> argument str (metavar "Name")
                                          <*> argument str (metavar "Path")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Upload a file." )
                       <> command "download-file"
                                  (withInfo ( doDownloadFile
                                          <$> argument str (metavar "Name")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Download a file." )
                       <> command "do-Transaction"
                                  (withInfo ( doTransaction
                                          <$> serverIpOption
                                          <*> serverPortOption) "Start a Transaction." )
                       <> command "lock-file"
                                  (withInfo ( doDownloadFile
                                          <$> argument str (metavar "Name")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Lock a file." )
                       <> command "unlock-file"
                                  (withInfo ( doDownloadFile
                                          <$> argument str (metavar "Name")
                                          <*> serverIpOption
                                          <*> serverPortOption) "unLock a file." )
                       ))
             (  fullDesc
             <> progDesc (progName ++ " is a simple test client for the use-haskell service." ++
                          " Try " ++ whiteCode ++ progName ++ " --help " ++ resetCode ++ " for more information. To " ++
                          " see the details of any command, " ++  "try " ++ whiteCode ++ progName ++ " COMMAND --help" ++
                          resetCode ++ ". The application supports bash completion. To enable, " ++
                          "ensure you have bash-completion installed and enabled (see your OS for details), the " ++
                          whiteCode ++ progName ++ resetCode ++
                          " application in your PATH, and place the following in your ~/.bash_profile : " ++ whiteCode ++
                          "source < (" ++ progName ++ " --bash-completion-script `which " ++ progName ++ "`)" ++
                          resetCode )
             <> header  (redCode ++ ", branch: " ++ resetCode))

-- helpers to simplify the creation of command line options
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                     <> short 'i'
                                     <> metavar "IPADDRESS"
                                     <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")



-- | function to build the client environment for performing a servant client rest call
-- It uses the host name and port parameters if Just x, or else uses envrionment variables
-- This uses an applicative programming style that is very condensed, and easy to understand when you get used to it,
-- compared to the alternative sequence of calls and subsequent record construction.

-- env1 :: String -> String -> IO SC.ClientEnv
-- env1 host port = SC.ClientEnv <$> newManager defaultManagerSettings
--                                                <*> (SC.BaseUrl <$> pure SC.Http
--                                                                <*> (host)
--                                                                <*> (read <$> (port))
--                                                                <*> pure "")
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

  -- pipe <- connect (host "127.0.0.1")
  -- ret <- access pipe master "filesystem" act
  -- close pipe
  -- return ret
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



