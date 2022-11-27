{-# LANGUAGE OverloadedStrings #-}

module Controllers.CommentControllerSpec (spec) where

import Constructors (makeId, makeUtc, makeUuid)
import Controllers.CommentController (createComment, getComment, getComments)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.UUID (nil)
import qualified Dto.CommentDto as CommentDto
import Dto.NewCommentDto (NewCommentDto(..))
import qualified Dto.NewCommentDto as NewCommentDto
import Mocks.CommentStore ()
import Mocks.StorageMock (runMock)
import qualified Mocks.StorageMock as StorageMock
import Models.Comment (Comment(..))
import qualified Models.Comment as Comment
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

  describe "Given a blog with no comments" $ do
    let noComments = StorageMock.emptyStorage {StorageMock.posts = posts}

    it "Does not find a single comment" $ do
      runMock (getComments Nothing) noComments `shouldReturn` []

    context "When creating a comment" $ do
      let
        newComment = NewCommentDto
          { NewCommentDto.title = "Title"
          , NewCommentDto.content = "Content"
          , NewCommentDto.postId = fstId & \(Id uuid) -> uuid
          , NewCommentDto.authorId = makeUuid "3034bb25-e47b-41ff-902c-5ab6aae7e6a6"
          , NewCommentDto.createdAt = makeUtc "2022-09-12 00:00"
          , NewCommentDto.updatedAt = makeUtc "2022-09-12 00:00"
          }

      it "Creates the first comment" $ do
        comments <- runMock
          (createComment newComment *> getComments Nothing) noComments
        length comments `shouldSatisfy` (== 1)

      it "Finds the comment" $ do
        comment <- runMock (createComment newComment >>= getComment) noComments
        CommentDto.title comment `shouldBe` NewCommentDto.title newComment
        CommentDto.content comment `shouldBe` NewCommentDto.content newComment
        CommentDto.authorId comment `shouldBe` NewCommentDto.authorId newComment

  describe "Given a blog with comments" $ do
    let
      fstCommentId = makeId "3034bb25-e47b-41ff-902c-5ab6aae7e6a6"
      commentUser = makeId "0a6c8791-ab24-4b87-8289-411582c3bab7"
      fstComment = Comment
        { Comment.title = "Comment 1"
        , Comment.content = "Comment content 1"
        , Comment.userId = commentUser
        , Comment.postId = fstId
        , Comment.createdAt = makeUtc "2022-09-12 00:00"
        , Comment.updatedAt = makeUtc "2022-09-12 00:00"
        }
      sndCommentId = makeId "c22d7bbb-e6d5-45bc-88d6-8a75db4b56a3"
      sndComment = Comment
        { Comment.title = "Comment 2"
        , Comment.content = "Comment content 2"
        , Comment.userId = commentUser
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
      runMock (length <$> getComments Nothing) givenComments `shouldReturn` 2

    it "Finds all posts by post" $ do
      commentsInPost <- runMock (getComments (Just sndId)) givenComments
      length commentsInPost `shouldBe` 1
      commentsInPost `shouldSatisfy` all ((== sndId) . Id . CommentDto.postId)

    context "When finding by id" $ do
      it "Throws error if not found" $ do
        runMock (getComment (Id nil)) givenComments `shouldThrow` anyException