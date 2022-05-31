{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT, MonadReader, MonadIO)
import Servant (Handler, ServerError)
import Control.Monad.Except (ExceptT, MonadError)
import Database.PostgreSQL.Simple
import Data.ByteString
import Data.Pool
import Control.Exception (bracket)

newtype AppT m a = AppT {
    runApp :: ReaderT Config (ExceptT ServerError m) a
} deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO)

newtype Config = Config {getPool :: Connection}
type DBConnectionString = ByteString


defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined}

connStr :: DBConnectionString
connStr = "host=localhost dbname=test user=postgres password=password port=5432"

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe