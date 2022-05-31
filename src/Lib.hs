{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Lib
where

import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp 
import Servant
import GHC.Generics


-- Postgresql Requires
import Database.PostgreSQL.Simple
import Control.Arrow hiding (app)
import Control.Monad.IO.Class

-- USER TYPE

data User = User
  { username      :: String
  , password      :: String
  , email         :: String
  } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User


toUsers :: [( String, String, String)] -> [User]
toUsers = map(\(username, password, email) -> User username password email)

-- Endpoints

type UserAPI = "users" :> Get '[JSON][User]
  :<|> "users" :> Capture "name" [Char] :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "users" :> "update" :>  ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "users" :> "delete" :>  ReqBody '[JSON] User :> Post '[JSON] User


-- ACTIONS FOR ENDPOINT

-- server :: Connection -> Server UserAPI
-- server conn = fetchAll :<|> fetch :<|> update :<|> delete 


-- DB Manipulation


type DBConnectionString = ByteString

initDB2 :: DBConnectionString -> IO ()
initDB2 connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS users (userId integer GENERATED ALWAYS AS IDENTITY PRIMARY KEY, username VARCHAR( 50 ) UNIQUE NOT NULL, password VARCHAR ( 50 ) NOT NULL, email VARCHAR ( 255 ) UNIQUE NOT NULL)"
  return ()