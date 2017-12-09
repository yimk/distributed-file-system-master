{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module AESLib where 

-- to fully implement cryptography, simply edit these functions
encryption :: a -> String -> a
encryption str key = str 

decryption :: a -> String -> a
decryption str key = str
        
keygenerator :: (String,String)
keygenerator = ("tmp_pbk","tmp_pvk")
