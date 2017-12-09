{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module RestAuthAPI where -- (getUsers, getUser, getPackages, Package(..))  where


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
import           AESLib


deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String 

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


data ServerKey = ServerKey
  { server_id :: Integer
  , server_public_key :: String -- encrpyted with CLIENT PVK ,needs CLIENT PBK
  }

data ClientKey = ClientKey
  { client_id :: Integer
  , client_public_key :: String -- encrpyted with CLIENT PVK ,needs CLIENT PBK

  }

type API = "requestTicket" :> ReqBody '[JSON] TicketRequest :> Get '[JSON] TicketResponse 



-- Constant

authenticationServerHost = "localhost"
authenticationServerPort = "4001"

serverKeyList :: [ServerKey]
serverKeyList =
  [ ServerKey 0 "server0_pbk" 
  , ServerKey 1 "server1_pbk"  
  , ServerKey 2 "server2_pbk"  
  ]

clientKeyList :: [ClientKey]
clientKeyList =
  [ ClientKey 0 "client0_pbk" 
  , ClientKey 1 "client1_pbk"  
  , ClientKey 2 "client2_pbk"  
  ]










