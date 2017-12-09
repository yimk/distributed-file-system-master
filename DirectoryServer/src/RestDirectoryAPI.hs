{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module RestDirectoryAPI where -- (getUsers, getUser, getPackages, Package(..))  where


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

data FileServer = FileServer
  { server_id :: Integer
  , server_host :: String 
  , server_port :: String 
  }

data DirectoryResponse = DirectoryResponse 
    { directory_response_file_name :: String
    , directory_response_assigned_file_name :: String
    , directory_response_host  :: String
    , directory_response_port :: String
    , directory_response_server_id :: String
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

type API = "getUploadDirectory" :> ReqBody '[JSON] String :> Get '[JSON] [DirectoryResponse] 
        :<|> "getDownloadDirectory" :> ReqBody '[JSON] String :> Get '[JSON] [DirectoryResponse] 




-- Constant 


directoryServerHost = "localhost"
directoryServerPort = "4000"


serverList :: [FileServer]
serverList =
  [ FileServer 0 "localhost" "8000"     -- Primary File Server
  , FileServer 1 "localhost" "8001"     -- Secondary File Server
  , FileServer 2 "localhost" "8002"     -- Secondary File Server
  ]










