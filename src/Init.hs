{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments #-}
module Init where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (liftIO, ReaderT (runReaderT))
import Config


-- appToServer cfg = hoistServer api (convertApp cfg) urlServer

-- convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- startApp :: IO ()
-- startApp = do
--     pool <- makePool
--     let cfg = defaultConfig {getPool = pool}
--     runSqlPool doMigrations pool
--     run 8080 $ app cfg

-- app :: Config -> Application
-- app cfg = serve api $ appToServer cfg