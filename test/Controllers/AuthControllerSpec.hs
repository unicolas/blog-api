{-# LANGUAGE OverloadedStrings #-}

module Controllers.AuthControllerSpec (spec) where

import Controllers.AuthController (LoginRequest(LoginRequest), login)
import qualified Controllers.AuthController as LoginRequest
import Data.ByteString.UTF8 (toString)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Mocks.AppMock (runMock)
import qualified Mocks.AppMock as AppMock
import Mocks.UserStore ()
import Models.Credentials (Credentials(Credentials))
import qualified Models.Credentials as Credentials
import Models.Types.Entity (Entity(..))
import Models.User (User(..))
import qualified Models.User as User
import qualified Servant as Http (getHeaders)
import qualified Servant.Auth.Server as Sas
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)
import Test.Hspec
  ( Spec
  , anyException
  , context
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  , shouldThrow
  )
import Utils (makeId)

spec :: Spec
spec = do
  describe "Given a blog with users" $ do
    let
      aUserId = makeId "cb97ab07-8785-4f03-9ead-a2178c680ec2"
      aUser = User
        { User.username = "username"
        , User.email = "name@mail.com"
        }
      users = Map.fromList [(aUserId, Entity aUserId aUser)]
      userPsw = "$2b$10$7gx1uWCIGJHmLpQAXqYoQOVSJqkfDAOrdvZsJpuBiZtdpcCVy4ClG"
      someCreds = Credentials
        { Credentials.userId = aUserId
        , Credentials.password = userPsw
        }
      credentials = Map.fromList [(aUserId, someCreds)]
      givenUsers = AppMock.emptyStorage
        { AppMock.users = users
        , AppMock.credentials = credentials
        }

    context "When providing correct login credentials" $ do
      let
        request = LoginRequest
          { LoginRequest.username = "username"
          , LoginRequest.password = "pass"
          }
      it "Accepts login" $ do
        key <- Sas.generateKey
        let check = login defaultCookieSettings (defaultJWTSettings key)
        response <- runMock (check request) givenUsers
        let headers = Http.getHeaders response
        length headers `shouldBe` 2
        headers `shouldSatisfy` all ((== "Set-Cookie") . fst)
        headers `shouldSatisfy` any (isPrefixOf "JWT-Cookie" . toString . snd)
        headers `shouldSatisfy` any (isPrefixOf "XSRF-TOKEN" . toString . snd)

    context "When providing invalid login credentials" $ do
      let
        request = LoginRequest
          { LoginRequest.username = "username"
          , LoginRequest.password = "wrong-password"
          }
      it "Rejects login" $ do
        key <- Sas.generateKey
        let check = login defaultCookieSettings (defaultJWTSettings key)
        runMock (check request) givenUsers `shouldThrow` anyException
