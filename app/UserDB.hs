{-# LANGUAGE OverloadedStrings #-}

module UserDB where

import qualified Database.PostgreSQL.Simple as Pg
import Data.Pool (Pool,withResource)
