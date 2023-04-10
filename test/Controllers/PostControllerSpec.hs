{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.PostControllerSpec (spec) where

import Controllers.PostController (createPost, deletePost, getPost, getPosts)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.UUID (nil)
import qualified Dto.Page as Page
import Dto.PostDto (NewPostDto(NewPostDto))
import qualified Dto.PostDto as NewPostDto (NewPostDto(..))
import qualified Dto.PostDto as PostDto (PostDto(..))
import qualified Dto.PostDto as PostIdDto (toPostId)
import Mocks.AppMock (runMock)
import qualified Mocks.AppMock as AppMock
import Mocks.PostStore ()
import Models.Post (Post(Post))
import qualified Models.Post as Post
import qualified Models.Types.Cursor as Cursor
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
  , shouldNotBe
  , shouldReturn
  , shouldSatisfy
  , shouldThrow
  )
import Utils (emptyPage, getUuid, makeId, makeUtc, serverError)

spec :: Spec
spec = do
  let
    idUser = makeId "0a6c8791-ab24-4b87-8289-411582c3bab7"
    anyUser = Nothing
    defaultSort = Nothing
    defaultOrder = Nothing
    defaultPageSize = Nothing
    fromFirst = Nothing
    sortTitle = Just Sort.Title
    sortCreated = Just Sort.CreatedAt
    orderAsc = Just Order.Asc
    orderDesc = Just Order.Desc
  let ?requestCtx = RequestContext {RequestContext.userId = idUser}

  describe "Given a blog with no posts" $ do
    let noPosts = AppMock.emptyStorage

    it "Does not find a single post" $ do
      let
        getAll = getPosts
          anyUser
          defaultSort
          defaultOrder
          fromFirst
          defaultPageSize
      runMock getAll noPosts `shouldReturn` emptyPage

    context "When creating a post" $ do
      let
        newPost = NewPostDto
          { NewPostDto.title = "Title"
          , NewPostDto.content = "Content"
          }

      it "Creates the first post" $ do
        let
          getAll = getPosts
            anyUser
            defaultSort
            defaultOrder
            fromFirst
            defaultPageSize
          createThenGet = createPost newPost *> getAll
        runMock (length . Page.content <$> createThenGet) noPosts `shouldReturn` 1

      it "Finds the post" $ do
        post <- runMock
          (createPost newPost >>= getPost . PostIdDto.toPostId)
          noPosts
        PostDto.title post `shouldBe` NewPostDto.title newPost
        PostDto.content post `shouldBe` NewPostDto.content newPost
        PostDto.authorId post `shouldBe` getUuid idUser

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
      thirdId = makeId "73292891-8728-4690-86ec-dc09d31dcca3"
      thirdPost = Post
        { Post.title = "Title 3"
        , Post.content = "Content 3"
        , Post.userId = fstUser
        , Post.createdAt = makeUtc "2022-09-18 00:00"
        , Post.updatedAt = makeUtc "2022-09-18 00:00"
        }
      posts = Map.fromList
        [ (fstId, Entity fstId fstPost)
        , (sndId, Entity sndId sndPost)
        , (thirdId, Entity thirdId thirdPost)
        ]
      givenPosts = AppMock.emptyStorage {AppMock.posts = posts}

    it "Finds all posts" $ do
      let
        getAll = getPosts
          anyUser
          defaultSort
          defaultOrder
          fromFirst
          defaultPageSize
      runMock (length . Page.content <$> getAll) givenPosts `shouldReturn` 3

    it "Finds all posts sorted by title descending" $ do
      let
        getAll = getPosts anyUser sortTitle orderDesc fromFirst defaultPageSize
        expectedTitles = Post.title <$> [thirdPost, sndPost, fstPost]
      page <- runMock getAll givenPosts
      PostDto.title <$> Page.content page `shouldBe` expectedTitles
      Page.hasNextPage page `shouldBe` False
      Page.nextCursor page `shouldBe` Nothing
      Page.pageSize page `shouldBe` Page.defaultPageSize

    it "Finds all posts sorted by created-at ascending" $ do
      let
        getAll = getPosts anyUser sortCreated orderAsc fromFirst defaultPageSize
        expectedDates = Post.createdAt <$> [fstPost, sndPost, thirdPost]
      page <- runMock getAll givenPosts
      PostDto.createdAt <$> Page.content page `shouldBe` expectedDates
      Page.hasNextPage page `shouldBe` False
      Page.nextCursor page `shouldBe` Nothing
      Page.pageSize page `shouldBe` Page.defaultPageSize

    it "Finds all posts by author" $ do
      let
        getByAuthor = getPosts
          (Just sndUser)
          defaultSort
          defaultOrder
          fromFirst
          defaultPageSize
      authoredPosts <- runMock (Page.content <$> getByAuthor) givenPosts
      length authoredPosts `shouldBe` 1
      authoredPosts `shouldSatisfy` all ((== sndUser) . Id . PostDto.authorId)

    context "When paging by 2" $ do
      let getAll = getPosts anyUser sortTitle orderDesc fromFirst (Just 2)

      it "Finds first 2 posts sorted by title descending" $ do
        let expectedTitles = Post.title <$> [thirdPost, sndPost]
        page <- runMock getAll givenPosts
        PostDto.title <$> Page.content page `shouldBe` expectedTitles
        Page.hasNextPage page `shouldBe` True
        Page.nextCursor page `shouldNotBe` Nothing
        Page.pageSize page `shouldBe` 2

      it "Finds left post on next page" $ do
        cursor <- runMock getAll givenPosts <&> Just
          . fromRight undefined
          . Cursor.decode
          . fromJust
          . Page.nextCursor
        let
          getNext = getPosts anyUser sortTitle orderDesc cursor (Just 2)
          expectedTitles = Post.title <$> [fstPost]
        page <- runMock getNext givenPosts
        PostDto.title <$> Page.content page `shouldBe` expectedTitles
        Page.hasNextPage page `shouldBe` False
        Page.nextCursor page `shouldBe` Nothing
        Page.pageSize page `shouldBe` 2

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
