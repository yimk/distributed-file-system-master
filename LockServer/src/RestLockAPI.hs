{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module RestLockAPI where -- (getUsers, getUser, getPackages, Package(..))  where


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

data FileInformation = FileInformation
  { storing_server_id :: String
  , file_assigned_name :: String
  } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON)


data LockItem = LockItem
  { lock_item_server :: String
  , lock_item_file :: String
  }

type API = "enableLock" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool
      :<|> "disableLock" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool 
      :<|> "isLocked" :> ReqBody '[JSON] FileInformation :> Get '[JSON] Bool 




-- Constant 

lockServerHost = "localhost"
lockServerPort = "4002"













