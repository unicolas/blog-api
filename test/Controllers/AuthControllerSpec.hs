{-# LANGUAGE OverloadedStrings #-}

module Controllers.AuthControllerSpec (spec) where

import Auth (generateKey)
import AuthClaims (refreshClaims)
import Controllers.AuthController
  (LoginRequest(LoginRequest), login, refreshToken)
import qualified Controllers.AuthController as LoginRequest (LoginRequest(..))
import qualified Controllers.AuthController as LoginResponse (LoginResponse(..))
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import Data.UUID (nil)
import Mocks.AppMock (runMock)
import qualified Mocks.AppMock as AppMock
import Mocks.UserStore ()
import Models.Credentials (Credentials(Credentials))
import qualified Models.Credentials as Credentials
import Models.Email (unsafeEmail)
import Models.HashedPassword (unsafeHashedPassword)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User(..))
import qualified Models.User as User
import Models.Username (unsafeUsername)
import Test.Hspec
  (Spec, anyException, context, describe, it, shouldSatisfy, shouldThrow)
import Utils (makeId)

spec :: Spec
spec = do
  describe "Given a blog with users" $ do
    let
      aUserId = makeId "cb97ab07-8785-4f03-9ead-a2178c680ec2"
      aUser = User
        { User.username = unsafeUsername "username"
        , User.email = unsafeEmail "name@mail.com"
        }
      users = Map.fromList [(aUserId, Entity aUserId aUser)]
      userPsw = "$2b$10$7gx1uWCIGJHmLpQAXqYoQOVSJqkfDAOrdvZsJpuBiZtdpcCVy4ClG"
      someCreds = Credentials
        { Credentials.userId = aUserId
        , Credentials.password = unsafeHashedPassword userPsw
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
        key <- generateKey
        response <- runMock (login key request) givenUsers
        LoginResponse.access response `shouldSatisfy` not . null
        LoginResponse.refresh response `shouldSatisfy` not . null

    context "When providing invalid login credentials" $ do
      let
        request = LoginRequest
          { LoginRequest.username = "username"
          , LoginRequest.password = "wrong-password"
          }
      it "Rejects login" $ do
        key <- generateKey
        let check = login key
        runMock (check request) givenUsers `shouldThrow` anyException

    context "When providing a valid refresh token" $ do
      it "Refreshes token" $ do
        key <- generateKey
        now <- getCurrentTime
        let claims = Just (refreshClaims (Id nil) now)
        response <- runMock (refreshToken key claims) givenUsers
        LoginResponse.access response `shouldSatisfy` not . null
        LoginResponse.refresh response `shouldSatisfy` not . null

    context "When providing an invalid refresh token" $ do
      it "Fails to refresh token" $ do
        key <- generateKey
        runMock (refreshToken key Nothing) givenUsers `shouldThrow` anyException
