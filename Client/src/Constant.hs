{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- For each Client, ensure have unique
  -- clientID
  -- host 4001

module Constant where -- (getUsers, getUser, getPackages, Package(..))  where

directoryServerHost = "localhost"
directoryServerPort = "4000"
authenticationServerHost = "localhost"
authenticationServerPort = "4001"
lockServerHost = "localhost"
lockServerPort = "4002"
clientID = "1"
myPvk = "mypvk"


transactionServerHost = "localhost"
transactionServerPort = "5000"