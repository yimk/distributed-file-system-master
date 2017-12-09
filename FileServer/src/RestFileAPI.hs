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

module RestFileAPI where -- (getUsers, getUser, getPackages, Package(..))  where


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
  { pvk_key_for_downloading :: String   -- tmp private key holding by file server during the transfer, needs to be decrypted by   file server's private key
                                        -- used to encrypt response file data

  , download_file_name :: String -- needs to be decrypted by file server's private key
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileDownloadResponse = FileDownloadResponse
  { download_file_data :: String 
    -- encrypted by tmp private key on file server side (this side)
    -- needs to be decrypted by tmp public key on client side
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileUploadRequest = FileUploadRequest
  { pvk_key_for_uploading :: String      -- tmp private key holding by file server during the transfer, needs to be decrypted by file server's private key
  , upload_file_name :: String
  , upload_file_data :: String -- needs to be decrypted by tmp private key
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)




---------------------------------------------------------




data TransactionUploadRequestToFileServer = TransactionUploadRequestToFileServer
  { turf_pvk_key :: String      -- tmp private key holding by file server during the transfer, needs to be decrypted by file server's private key
  , turf_file_name :: String
  , turf_file_data :: String -- needs to be decrypted by tmp private key
  , turf_id :: String           
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data ShadowRecord = ShadowRecord
  { shadow_record_tid :: String
  , shadow_record_file_name :: String
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)





---------------------------------------------------------
type API = "request_download_file" :> ReqBody '[JSON] FileDownloadRequest :> Get '[JSON] FileDownloadResponse
        :<|> "request_upload_file" :> ReqBody '[JSON] FileUploadRequest :> Post '[JSON] Bool
        :<|> "request_ready_to_commit_response" :> Get '[JSON] Bool
        :<|> "request_shadowing_upload" :> ReqBody '[JSON] TransactionUploadRequestToFileServer :> Post '[JSON] Bool
        :<|> "request_transaction_complete" :> ReqBody '[JSON] String :> Post '[JSON] Bool
        


-- Constant 

filePath = "/FileSystem/" 
shadowFilePath = "/shadowFileSystem/"             
fileServerHost = "localhost"
fileServerPort = "8000"

serverID = "1"
myPvk = "mypvk"             -- no need to change when duplicate














