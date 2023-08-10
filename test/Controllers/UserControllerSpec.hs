{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.UserControllerSpec (spec) where

import Controllers.UserController (createUser, getUser)
import qualified Data.Map as Map
import Data.UUID (nil)
import Dto.UserDto (NewUserDto(NewUserDto))
import qualified Dto.UserDto as NewUserDto (NewUserDto(..))
import qualified Dto.UserDto as UserDto (UserDto(..))
import qualified Dto.UserDto as UserIdDto (UserIdDto(..))
import Mocks.AppMock (runMock)
import qualified Mocks.AppMock as AppMock
import Mocks.UserStore ()
import Models.Email (unsafeEmail)
import Models.Password (unsafePassword)
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Models.User (User(..))
import qualified Models.User as User
import Models.Username (unsafeUsername)
import RequestContext (RequestContext(..))
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldThrow)
import Utils (makeId, serverError)

spec :: Spec
spec = do
  let idUser = makeId "b73894f9-39e0-427a-abb4-48ff7322d3ab"
  let ?requestCtx = RequestContext {RequestContext.userId = idUser}

  describe "Given a blog with users" $ do
    let
      aUserId = makeId "cb97ab07-8785-4f03-9ead-a2178c680ec2"
      aUser = User
        { User.username = unsafeUsername "username"
        , User.email = unsafeEmail "name@mail.com"
        }
      users = Map.fromList [(aUserId, Entity aUserId aUser)]
      givenUsers = AppMock.emptyStorage {AppMock.users = users}

    context "When finding by id" $ do
      it "Finds the user" $ do
        user <- runMock (getUser aUserId) givenUsers
        UserDto.username user `shouldBe` User.username aUser
        UserDto.email user `shouldBe` User.email aUser

      it "Throws error if not found" $ do
        runMock (getUser (Id nil)) givenUsers `shouldThrow` serverError 404

    context "When creating a user" $ do
      it "Finds the new user" $ do
        let
          newUser = NewUserDto
            { NewUserDto.username = unsafeUsername "new_user"
            , NewUserDto.email = unsafeEmail "new_u@mail.com"
            , NewUserDto.password = unsafePassword "abc123$"
            }
          createThenGet = createUser newUser >>= getUser . Id . UserIdDto.userId
        user <- runMock createThenGet givenUsers
        UserDto.username user `shouldBe` NewUserDto.username newUser
        UserDto.email user `shouldBe` NewUserDto.email newUser

      it "Throws error if username already exists" $ do
        let
          newUser = NewUserDto
            { NewUserDto.username = unsafeUsername "username"
            , NewUserDto.email = unsafeEmail "new_u@mail.com"
            , NewUserDto.password = unsafePassword "abc123$"
            }
        runMock (createUser newUser) givenUsers `shouldThrow` serverError 400
