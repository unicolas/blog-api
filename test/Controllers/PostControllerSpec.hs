{-# LANGUAGE OverloadedStrings #-}

module Controllers.PostControllerSpec (spec) where

import Controllers.PostController (createPost, getPost, getPosts)
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.UUID (UUID, fromString, nil)
import Dto.PostDto (PostDto(..))
import Mocks.PostStore (Posts(Posts), runMock)
import Models.Post (Post(Post))
import qualified Models.Post as Post
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import Test.Hspec
  ( Spec
  , anyException
  , context
  , describe
  , it
  , shouldBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )

spec :: Spec
spec = do
  describe "Given a blog with no posts" $ do
    let noPosts = Posts []

    it "Does not find a single post" $ do
      runMock (getPosts Nothing) noPosts `shouldReturn` []

    context "When creating a post" $ do
      let
        postDto = PostDto
          { postId = nil
          , title = "Title"
          , content = "Content"
          , authorId = makeUuid "3034bb25-e47b-41ff-902c-5ab6aae7e6a6"
          , createdAt = makeUtc "2022-09-12 00:00"
          , updatedAt = makeUtc "2022-09-12 00:00"
          }

      it "Creates the first post" $ do
        posts <- runMock (createPost postDto *> getPosts Nothing) noPosts
        length posts `shouldSatisfy` (== 1)

      it "Finds the post" $ do
        post <- runMock (createPost postDto >>= getPost) noPosts
        title post `shouldBe` title postDto
        content post `shouldBe` content postDto
        authorId post `shouldBe` authorId postDto

  describe "Given a blog with posts" $ do
    let
      fstId = makeId "3034bb25-e47b-41ff-902c-5ab6aae7e6a6"
      fstUser = makeId "0a6c8791-ab24-4b87-8289-411582c3bab7"
      fstPost = Post
        { Post.title = "Title 1"
        , Post.content = "Content 1"
        , Post.userId = fstUser
        , Post.createdAt = makeUtc "2022-09-12 00:00"
        , Post.updatedAt = makeUtc "2022-09-12 00:00"
        }
      sndId = makeId "c22d7bbb-e6d5-45bc-88d6-8a75db4b56a3"
      sndUser = makeId "f0fca0d8-085b-463d-974d-f4f4a16cb305"
      sndPost = Post
        { Post.title = "Title 2"
        , Post.content = "Content 2"
        , Post.userId = sndUser
        , Post.createdAt = makeUtc "2022-09-17 00:00"
        , Post.updatedAt = makeUtc "2022-09-17 00:00"
        }
      givenPosts = Posts
        [ (fstId, Entity fstId fstPost)
        , (sndId, Entity sndId sndPost)
        ]

    it "Finds all posts" $ do
      runMock (length <$> getPosts Nothing) givenPosts `shouldReturn` 2

    it "Finds all posts by author" $ do
      runMock (length <$> getPosts (Just sndUser)) givenPosts `shouldReturn` 1

    context "When finding by id" $ do
      it "Throws error if not found" $ do
        runMock (getPost (Id nil)) givenPosts `shouldThrow` anyException


makeUtc :: String -> UTCTime
makeUtc = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"

makeUuid :: String -> UUID
makeUuid = fromJust . fromString

makeId :: String -> Id phantom
makeId = Id . makeUuid
