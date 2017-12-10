{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module RestTransactionAPI where -- (getUsers, getUser, getPackages, Package(..))  where


import           Control.Applicative
import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant


deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

data TransactionRecord = TransactionRecord
  { tr_id :: String
  , tr_sid :: String
  , tr_host :: String
  , tr_port :: String
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileInformation = FileInformation
  { storing_server_id :: String
  , file_assigned_name :: String
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data DirectoryResponse = DirectoryResponse 
    { directory_response_file_name :: String
    , directory_response_assigned_file_name :: String
    , directory_response_host  :: String
    , directory_response_port :: String
    , directory_response_server_id :: String
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data TicketResponse = TicketResponse 
    { ticket_response_tmp_pbk :: String -- encrypted with CLIENT PBK, generated for temporary file transformation, quite expensive
      -- for file server
    , ticket_response_tmp_pvk   :: String -- encrypted with FILE SERVER PBK, generated for temporary file transformation
    , ticket_response_file_name  :: String -- encrypted with FILE SERVER PBK
    } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)


data TicketRequest = TicketRequest
  { ticket_request_client_id :: String -- transparent     
  , ticket_request_target_server_id :: String -- encrpyted with CLIENT PVK ,needs CLIENT PBK
  , ticket_request_target_file_name :: String -- encrpyted with CLIENT PVK ,needs CLIENT PBK
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data TransactionUploadRequestFromClient = TransactionUploadRequestFromClient 
    { turc_file_name :: String
    , turc_file_data  :: String
    , turc_transaction_id :: String
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data TransactionUploadRequestToFileServer = TransactionUploadRequestToFileServer
  { turf_pvk_key :: String      -- tmp private key holding by file server during the transfer, needs to be decrypted by file server's private key
  , turf_file_name :: String
  , turf_file_data :: String -- needs to be decrypted by tmp private key
  , turf_id :: String
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data TransactionLog = TransactionLog
  { completed_transaction :: String
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

type API = "start_transaction_request" :> ReqBody '[JSON] String :> Get '[JSON] String            -- param: filename 
                                                                                                  -- return: transaction id
        :<|> "end_transaction_request" :> ReqBody '[JSON] String :> Get '[JSON] Bool
        :<|> "transaction_upload" :> ReqBody '[JSON] TransactionUploadRequestFromClient :> Get '[JSON] Bool                   -- upload 
                                                                                                  -- either create or edit
                                                                                                  -- the directory server identifies the file name
                                                                                                    -- exist -> edit (return old directory and old file will end up be overwrote)
                                                                                                    -- new -> create


-- as a server that send request
type FileServerAPI = "getUploadDirectory" :> ReqBody '[JSON] String :> Get '[JSON] [DirectoryResponse]
        :<|> "requestTicket" :> ReqBody '[JSON] TicketRequest :> Get '[JSON] TicketResponse 
        :<|> "enableLock" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool
        :<|> "disableLock" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool 
        :<|> "isLocked" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool 
        :<|> "request_ready_to_commit_response" :> Get '[JSON] Bool
        :<|> "request_shadowing_upload" :> ReqBody '[JSON] TransactionUploadRequestToFileServer :> Post '[JSON] Bool
        :<|> "request_transaction_complete" :> ReqBody '[JSON] String :> Post '[JSON] Bool

restFileServerAPI :: Proxy FileServerAPI
restFileServerAPI = Proxy



getUploadDirectory :: String -> ClientM [DirectoryResponse]
requestTicket :: TicketRequest -> ClientM TicketResponse
enableLock :: FileInformation -> ClientM Bool
disableLock :: FileInformation -> ClientM Bool
isLocked :: FileInformation -> ClientM Bool
readyToCommitRequest :: ClientM Bool
shadowUploadRequest :: TransactionUploadRequestToFileServer -> ClientM Bool
transactionCompleteRequest :: String -> ClientM Bool


( getUploadDirectory :<|> 
  requestTicket :<|> 
  enableLock :<|> 
  disableLock :<|> 
  isLocked :<|> 
  readyToCommitRequest :<|> 
  shadowUploadRequest :<|> 
  transactionCompleteRequest 
  ) = client restFileServerAPI 


-- Constant 


transactionServerHost = "localhost"
transactionServerPort = "5000"



directoryServerHost = "localhost"
directoryServerPort = "4000"
authenticationServerHost = "localhost"
authenticationServerPort = "4001"
lockServerHost = "localhost"
lockServerPort = "4002"
clientID = "0"
myPvk = "mypvk"










