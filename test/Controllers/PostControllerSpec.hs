{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.PostControllerSpec (spec) where

import Constructors (makeId, makeUtc, serverError)
import Controllers.PostController (createPost, deletePost, getPost, getPosts)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.UUID (nil)
import Dto.NewPostDto (NewPostDto(NewPostDto))
import qualified Dto.NewPostDto as NewPostDto
import qualified Dto.PostDto as PostDto
import Mocks.PostStore ()
import Mocks.StorageMock (runMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Post (Post(Post))
import qualified Models.Post as Post
import Models.Types.Entity (Entity(..))
import Models.Types.Id (Id(..))
import qualified Models.Types.Sorting as Order (Order(Asc, Desc))
import qualified Models.Types.Sorting as Sort (Sort(CreatedAt, Title))
import RequestContext (RequestContext(..))
import Test.Hspec
  ( Spec
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
  let idUser = makeId "0a6c8791-ab24-4b87-8289-411582c3bab7"
  let ?requestCtx = RequestContext {RequestContext.userId = idUser}

  describe "Given a blog with no posts" $ do
    let noPosts = StorageMock.emptyStorage

    it "Does not find a single post" $ do
      let get = getPosts Nothing Nothing Nothing
      runMock get noPosts `shouldReturn` []

    context "When creating a post" $ do
      let
        newPost = NewPostDto
          { NewPostDto.title = "Title"
          , NewPostDto.content = "Content"
          }

      it "Creates the first post" $ do
        let
          createThenGet = createPost newPost *> getPosts Nothing Nothing Nothing
        runMock (length <$> createThenGet) noPosts `shouldReturn` 1

      it "Finds the post" $ do
        post <- runMock (createPost newPost >>= getPost) noPosts
        PostDto.title post `shouldBe` NewPostDto.title newPost
        PostDto.content post `shouldBe` NewPostDto.content newPost
        PostDto.authorId post `shouldBe` (idUser & \(Id uuid) -> uuid)

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
      posts = Map.fromList
        [ (fstId, Entity fstId fstPost)
        , (sndId, Entity sndId sndPost)
        ]
      givenPosts = StorageMock.emptyStorage {StorageMock.posts = posts}

    it "Finds all posts" $ do
      let get = getPosts Nothing Nothing Nothing
      runMock (length <$> get) givenPosts `shouldReturn` 2

    it "Finds all posts sorted by title descending" $ do
      let
        get = getPosts Nothing (Just Sort.Title) (Just Order.Desc)
        expectedTitles = [Post.title sndPost, Post.title fstPost]
      dtos <- runMock get givenPosts
      PostDto.title <$> dtos `shouldBe` expectedTitles

    it "Finds all posts sorted by created-at ascending" $ do
      let
        get = getPosts Nothing (Just Sort.CreatedAt) (Just Order.Asc)
        expectedDates = [Post.createdAt fstPost, Post.createdAt sndPost]
      dtos <- runMock get givenPosts
      PostDto.createdAt <$> dtos `shouldBe` expectedDates

    it "Finds all posts by author" $ do
      let getByAuthor = getPosts (Just sndUser) Nothing Nothing
      authoredPosts <- runMock getByAuthor givenPosts
      length authoredPosts `shouldBe` 1
      authoredPosts `shouldSatisfy` all ((== sndUser) . Id . PostDto.authorId)

    context "When finding by id" $ do
      it "Throws error if not found" $ do
        runMock (getPost (Id nil)) givenPosts `shouldThrow` serverError 404

    context "When deleting a post" $ do
      it "Does not find the post" $ do
        let deleteThenGet idPost = deletePost idPost *> getPost idPost
        runMock (deleteThenGet fstId) givenPosts `shouldThrow` serverError 404

      it "Throws error if not found" $ do
        runMock (deletePost (Id nil)) givenPosts `shouldThrow` serverError 404

      it "Throws error if authored by other user" $ do
        runMock (deletePost sndId) givenPosts `shouldThrow` serverError 403
