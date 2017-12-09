{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- For each Client, ensure have unique
  -- clientID
  -- host 4001

module RestClientAPI where -- (getUsers, getUser, getPackages, Package(..))  where


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


data FileDownloadRequest = FileDownloadRequest
  { pvk_key_for_downloading :: String      -- tmp private key holding by file server during the transfer, needs to be decrypted by file server's private key
  , download_file_name :: String -- needs to be decrypted by file server's private key
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileUploadRequest = FileUploadRequest
  { pvk_key_for_uploading :: String      -- tmp private key holding by file server during the transfer, needs to be decrypted by file server's private key
  , upload_file_name :: String
  , upload_file_data :: String -- needs to be decrypted by tmp private key
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileDownloadResponse = FileDownloadResponse
  { download_file_data :: String 
    -- encrypted by tmp private key on file server side (this side)
    -- needs to be decrypted by tmp public key on client side
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
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)


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

type API =	"request_download_file" :> ReqBody '[JSON] FileDownloadRequest :> Get '[JSON] FileDownloadResponse
        :<|> "request_upload_file" :> ReqBody '[JSON] FileUploadRequest :> Post '[JSON] Bool
	   	  :<|> "getDownloadDirectory" :> ReqBody '[JSON] String :> Get '[JSON] [DirectoryResponse]
       	:<|> "getUploadDirectory" :> ReqBody '[JSON] String :> Get '[JSON] [DirectoryResponse]
       	:<|> "requestTicket" :> ReqBody '[JSON] TicketRequest :> Get '[JSON] TicketResponse 
        :<|> "enableLock" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool
        :<|> "disableLock" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool 
        :<|> "isLocked" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool 
        :<|> "start_transaction_request" :> ReqBody '[JSON] String :> Get '[JSON] String          
        :<|> "end_transaction_request" :> ReqBody '[JSON] String :> Get '[JSON] Bool
        :<|> "transaction_upload" :> ReqBody '[JSON] TransactionUploadRequestFromClient :> Get '[JSON] Bool 


restAPI :: Proxy API
restAPI = Proxy

downloadFile :: FileDownloadRequest -> ClientM FileDownloadResponse
uploadFile :: FileUploadRequest -> ClientM Bool
getDownloadDirectory :: String -> ClientM [DirectoryResponse]
getUploadDirectory :: String -> ClientM [DirectoryResponse]
requestTicket :: TicketRequest -> ClientM TicketResponse
enableLock :: FileInformation -> ClientM Bool
disableLock :: FileInformation -> ClientM Bool
isLocked :: FileInformation -> ClientM Bool
startTransactionRequest :: String -> ClientM String
endTransactionRequest :: String -> ClientM Bool
transactionUpload :: TransactionUploadRequestFromClient -> ClientM Bool


( downloadFile :<|> 
  uploadFile :<|> 
  getDownloadDirectory :<|> 
  getUploadDirectory :<|> 
  requestTicket :<|> 
  enableLock :<|> 
  disableLock :<|> 
  isLocked :<|> 
  startTransactionRequest :<|> 
  endTransactionRequest :<|> 
  transactionUpload) = client restAPI 














