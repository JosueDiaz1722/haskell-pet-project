module Main where

import Data.ByteString (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple
import Control.Exception (bracket)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import Servant.Client
import Control.Concurrent
import Config (configPG, initConnectPool)
import Lib (runApp)
import Control.Monad


main :: IO ()
main = do
    conn <- initConnectPool configPG 
    runApp conn