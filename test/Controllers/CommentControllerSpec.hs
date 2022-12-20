{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.CommentControllerSpec (spec) where

import Constructors (makeId, makeUtc, serverError)
import Controllers.CommentController
  (createComment, deleteComment, getComment, getComments)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.UUID (nil)
import qualified Dto.CommentDto as CommentDto
import Dto.NewCommentDto (NewCommentDto(..))
import qualified Dto.NewCommentDto as NewCommentDto
import Mocks.CommentStore ()
import Mocks.PostStore ()
import Mocks.StorageMock (runMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Comment (Comment(..))
import qualified Models.Comment as Comment
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
    idUser = makeId "b73894f9-39e0-427a-abb4-48ff7322d3ab"
  let ?requestCtx = RequestContext {RequestContext.userId = idUser}

  describe "Given a blog with no comments" $ do
    let noComments = StorageMock.emptyStorage {StorageMock.posts = posts}

    it "Does not find a single comment" $ do
      let get = getComments Nothing Nothing Nothing
      runMock get noComments `shouldReturn` []

    context "When creating a comment" $ do
      let
        newComment = NewCommentDto
          { NewCommentDto.title = "Title"
          , NewCommentDto.content = "Content"
          , NewCommentDto.postId = fstId & \(Id uuid) -> uuid
          }

      it "Creates the first comment" $ do
        let
          createThenGet = createComment newComment
            *> getComments Nothing Nothing Nothing
        runMock (length <$> createThenGet) noComments `shouldReturn` 1

      it "Finds the comment" $ do
        comment <- runMock (createComment newComment >>= getComment) noComments
        CommentDto.title comment `shouldBe` NewCommentDto.title newComment
        CommentDto.content comment `shouldBe` NewCommentDto.content newComment
        CommentDto.authorId comment `shouldBe` (idUser & \(Id uuid) -> uuid)

      it "Throws error if commented post does not exist" $ do
        let newComment' = newComment{NewCommentDto.postId = nil}
        runMock (createComment newComment') noComments `shouldThrow` serverError 404

  describe "Given a blog with comments" $ do
    let
      fstCommentId = makeId "127c2982-355b-4e06-9313-b63db0d1aa49"
      fstCommentUser = makeId "b73894f9-39e0-427a-abb4-48ff7322d3ab"
      fstComment = Comment
        { Comment.title = "Comment 1"
        , Comment.content = "Comment content 1"
        , Comment.userId = fstCommentUser
        , Comment.postId = fstId
        , Comment.createdAt = makeUtc "2022-09-12 00:00"
        , Comment.updatedAt = makeUtc "2022-09-12 00:00"
        }
      sndCommentId = makeId "e1da0ad5-d7fb-4c80-bb53-999d6e7c6147"
      sndCommentUser = makeId "0a6c8791-ab24-4b87-8289-411582c3bab7"
      sndComment = Comment
        { Comment.title = "Comment 2"
        , Comment.content = "Comment content 2"
        , Comment.userId = sndCommentUser
        , Comment.postId = sndId
        , Comment.createdAt = makeUtc "2022-09-17 00:00"
        , Comment.updatedAt = makeUtc "2022-09-17 00:00"
        }
      comments = Map.fromList
        [ (fstCommentId, Entity fstCommentId fstComment)
        , (sndCommentId, Entity sndCommentId sndComment)
        ]
      givenComments = StorageMock.emptyStorage
        { StorageMock.posts = posts
        , StorageMock.comments = comments
        }

    it "Finds all comments" $ do
      let get = getComments Nothing Nothing Nothing
      runMock (length <$> get) givenComments `shouldReturn` 2

    it "Finds all comments sorted by title descending" $ do
      let
        get = getComments Nothing (Just Sort.Title) (Just Order.Desc)
        expectedTitles = [Comment.title sndComment, Comment.title fstComment]
      dtos <- runMock get givenComments
      CommentDto.title <$> dtos `shouldBe` expectedTitles

    it "Finds all posts sorted by created-at ascending" $ do
      let
        get = getComments Nothing (Just Sort.CreatedAt) (Just Order.Asc)
        expectedDates =
          [ Comment.createdAt fstComment
          , Comment.createdAt sndComment
          ]
      dtos <- runMock get givenComments
      CommentDto.createdAt <$> dtos `shouldBe` expectedDates

    it "Finds all posts by post" $ do
      let getBy = getComments (Just sndId) Nothing Nothing
      commentsInPost <- runMock getBy givenComments
      length commentsInPost `shouldBe` 1
      commentsInPost `shouldSatisfy` all ((== sndId) . Id . CommentDto.postId)

    context "When finding by id" $ do
      it "Throws error if not found" $ do
        runMock (getComment (Id nil)) givenComments `shouldThrow` serverError 404

    context "When deleting a comment" $ do
      it "Does not find the comment" $ do
        let deleteThenGet idComment = deleteComment idComment *> getComment idComment
        runMock (deleteThenGet fstCommentId) givenComments `shouldThrow` serverError 404

      it "Throws error if not found" $ do
        runMock (deleteComment (Id nil)) givenComments `shouldThrow` serverError 404

      it "Throws error if authored by other user" $ do
        runMock (deleteComment sndCommentId) givenComments `shouldThrow` serverError 403
