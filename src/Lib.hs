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
import Data.Time (UTCTime)

-- Postgresql Requires
import Database.PostgreSQL.Simple as Pg
import Control.Arrow hiding (app)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Control.Monad

-- USER TYPE

data User = User
  { userCreatedAt :: UTCTime 
  , username      :: String
  , password      :: String
  , email         :: String
  } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User


toUsers :: [( UTCTime, String, String, String)] -> [User]
toUsers = map(\(userCreatedAt, username, password, email) -> User userCreatedAt username password email)

-- Endpoints

type UserAPI = "users" :> Get '[JSON][User]
  -- :<|> "users" :> Capture "name" [Char] :> Get '[JSON] [User]
  -- :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  -- :<|> "users" :> "update" :>  ReqBody '[JSON] User :> Post '[JSON] User
  -- :<|> "users" :> "delete" :>  ReqBody '[JSON] User :> Post '[JSON] User


-- ACTIONS FOR ENDPOINT
api :: Proxy UserAPI
api = Proxy

server :: Pool Connection -> Server UserAPI
server conns = fetchAll

  where fetchAll :: Handler [User]
        fetchAll = liftIO $ getAllUsers conns
-- DB Manipulation

instance Pg.FromRow User where
  fromRow = User
    <$> field
    <*> field
    <*> field
    <*> field

instance Pg.ToRow User where
  toRow t = 
    [ toField (userCreatedAt t)
    , toField (username t)
    , toField (password t)
    , toField (email t)
    ]

getAllUsers :: Pool Pg.Connection -> IO [User]
getAllUsers pool =
  withResource pool $ \conn -> 
  Pg.query_ conn "SELECT created_at, username, password, email FROM users"

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)