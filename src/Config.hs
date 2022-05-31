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
import System.Environment (getEnv)

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

configPG :: ConnectInfo
configPG = ConnectInfo
    { connectHost     = "localhost" -- .env
    , connectDatabase = "test" -- .env
    , connectUser     = "testuser" -- .env
    , connectPassword = "password" -- .env
    , connectPort     = 5432 -- .env
    }

initConnectPool :: ConnectInfo -> IO (Pool Connection)
initConnectPool connInfo = do
    createPool (connect connInfo)
                close
                1
                60
                1