{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (runApp)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return runApp) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"email\":\"user1@gmail.com\",\"password\":\"password1\",\"userCreatedAt\":\"2022-05-31T18:58:21.785392Z\",\"username\":\"user1\"},{\"email\":\"user2@gmail.com\",\"password\":\"password2\",\"userCreatedAt\":\"2022-05-31T18:58:21.785392Z\",\"username\":\"user2\"}]"
            get "/users" `shouldRespondWith` users
