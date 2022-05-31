module Main where

import Data.ByteString (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple
import Control.Exception (bracket)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import Servant.Client
import Control.Concurrent


main :: IO ()
main = print("Hello")
