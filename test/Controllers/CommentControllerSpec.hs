{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.CommentControllerSpec (spec) where

import Controllers.CommentController
  ( createCommentReply
  , createPostComment
  , deleteComment
  , getComment
  , getCommentReplies
  , getPostComments
  )
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.UUID (nil)
import Dto.CommentDto (NewCommentDto(NewCommentDto))
import qualified Dto.CommentDto as CommentDto (CommentDto(..))
import qualified Dto.CommentDto as CommentIdDto (CommentIdDto(..))
import qualified Dto.CommentDto as NewCommentDto (NewCommentDto(..))
import qualified Dto.Page as Page
import Mocks.AppMock (runMock)
import qualified Mocks.AppMock as AppMock
import Mocks.CommentStore ()
import Mocks.PostStore ()
import Models.Comment (Comment(..))
import qualified Models.Comment as Comment
import Models.Post (Post(Post))
import qualified Models.Post as Post
import qualified Models.Types.Cursor as Cursor
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
import Utils (emptyPage, makeId, makeUtc, serverError)

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
    posts = AppMock.storeFromList [(fstId, fstPost), (sndId, sndPost)]
    idUser = makeId "b73894f9-39e0-427a-abb4-48ff7322d3ab"
    defaultSort = Nothing
    defaultOrder = Nothing
    defaultPageSize = Nothing
    fromFirst = Nothing
    sortTitle = Just Sort.Title
    sortCreated = Just Sort.CreatedAt
    orderAsc = Just Order.Asc
    orderDesc = Just Order.Desc
  let ?requestCtx = RequestContext {RequestContext.userId = idUser}

  describe "Given a blog with posts and no comments" $ do
    let noComments = AppMock.emptyStorage {AppMock.posts = posts}

    it "Does not find post comments" $ do
      let
        getAll = getPostComments
          fstId
          defaultSort
          defaultOrder
          fromFirst
          defaultPageSize
      runMock getAll noComments `shouldReturn` emptyPage

    context "When creating a comment" $ do
      let
        newComment = NewCommentDto
          { NewCommentDto.title = "Title"
          , NewCommentDto.content = "Content"
          }

      it "Creates the first comment" $ do
        let
          getAll = getPostComments
            fstId
            defaultSort
            defaultOrder
            fromFirst
            defaultPageSize
          createThenGet = createPostComment fstId newComment *> getAll
        runMock (length . Page.content <$> createThenGet) noComments `shouldReturn` 1

      it "Finds the comment" $ do
        comment <- runMock
          (createPostComment fstId newComment
            >>= getComment . CommentIdDto.commentId
          ) noComments
        CommentDto.title comment `shouldBe` NewCommentDto.title newComment
        CommentDto.content comment `shouldBe` NewCommentDto.content newComment
        CommentDto.authorId comment `shouldBe` idUser
        CommentDto.parentId comment `shouldBe` Nothing
        CommentDto.postId comment `shouldBe` fstId

      it "Throws error if commented post does not exist" $ do
        runMock (createPostComment (Id nil) newComment) noComments
          `shouldThrow` serverError 404

  describe "Given a blog with comments and replies" $ do
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
        , Comment.parentId = Nothing
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
        , Comment.parentId = Nothing
        }
      thirdCommentId = makeId "b17e8ffa-f8ca-4169-8338-6bfb3a16c24c"
      thirdComment = Comment
        { Comment.title = "Comment 3"
        , Comment.content = "Comment content 3"
        , Comment.userId = fstCommentUser
        , Comment.postId = sndId
        , Comment.createdAt = makeUtc "2022-09-10 00:00"
        , Comment.updatedAt = makeUtc "2022-09-10 00:00"
        , Comment.parentId = Nothing
        }
      fourthCommentId = makeId "11dba59c-f85d-45d8-a778-8405c6c61700"
      fourthComment = Comment
        { Comment.title = "Comment 4"
        , Comment.content = "Comment content 4"
        , Comment.userId = sndCommentUser
        , Comment.postId = sndId
        , Comment.createdAt = makeUtc "2022-10-03 00:00"
        , Comment.updatedAt = makeUtc "2022-10-03 00:00"
        , Comment.parentId = Nothing
        }
      fstReplyId = makeId "4cd4da9b-e53d-4649-9246-7ad5f4212b2f"
      fstReply = Comment
        { Comment.title = "Reply 1"
        , Comment.content = "Reply content 1"
        , Comment.userId = fstCommentUser
        , Comment.postId = sndId
        , Comment.createdAt = makeUtc "2022-09-23 00:00"
        , Comment.updatedAt = makeUtc "2022-09-23 00:00"
        , Comment.parentId = Just sndCommentId
        }
      sndReplyId = makeId "8ef2fd9b-5041-4173-a400-42a2944bb038"
      sndReply = Comment
        { Comment.title = "Reply 2"
        , Comment.content = "Reply content 2"
        , Comment.userId = sndCommentUser
        , Comment.postId = sndId
        , Comment.createdAt = makeUtc "2022-09-25 00:00"
        , Comment.updatedAt = makeUtc "2022-09-25 00:00"
        , Comment.parentId = Just sndCommentId
        }
      comments = AppMock.storeFromList
        [ (fstCommentId, fstComment)
        , (sndCommentId, sndComment)
        , (thirdCommentId, thirdComment)
        , (fourthCommentId, fourthComment)
        , (fstReplyId, fstReply)
        , (sndReplyId, sndReply)
        ]
      givenComments = AppMock.emptyStorage
        { AppMock.posts = posts
        , AppMock.comments = comments
        }

    it "Finds all comments" $ do
      let
        getBy = getPostComments
          sndId
          defaultSort
          defaultOrder
          fromFirst
          defaultPageSize
      commentsInPost <- runMock (Page.content <$> getBy) givenComments
      length commentsInPost `shouldBe` 3
      commentsInPost `shouldSatisfy` all ((== sndId) . CommentDto.postId)

    it "Finds all comments sorted by title descending" $ do
      let
        get = getPostComments sndId sortTitle orderDesc fromFirst defaultPageSize
        expectedTitles = Comment.title <$> [fourthComment, thirdComment, sndComment]
      page <- runMock get givenComments
      CommentDto.title <$> Page.content page `shouldBe` expectedTitles
      Page.hasNextPage page `shouldBe` False
      Page.nextCursor page `shouldBe` Nothing
      Page.pageSize page `shouldBe` Page.defaultPageSize

    it "Finds all comments sorted by created-at ascending" $ do
      let
        get = getPostComments sndId sortCreated orderAsc fromFirst defaultPageSize
        expectedDates = Comment.createdAt
          <$> [thirdComment, sndComment, fourthComment]
      page <- runMock get givenComments
      CommentDto.createdAt <$> Page.content page `shouldBe` expectedDates
      Page.hasNextPage page `shouldBe` False
      Page.nextCursor page `shouldBe` Nothing
      Page.pageSize page `shouldBe` Page.defaultPageSize

    context "When paging by 2" $ do
      let getAll = getPostComments sndId sortCreated orderDesc fromFirst (Just 2)

      it "Finds first 2 comments sorted by created-at descending" $ do
        let expectedTitles = Comment.title <$> [fourthComment, sndComment]
        page <- runMock getAll givenComments
        CommentDto.title <$> Page.content page `shouldBe` expectedTitles
        Page.hasNextPage page `shouldBe` True
        Page.nextCursor page `shouldNotBe` Nothing
        Page.pageSize page `shouldBe` 2

      it "Finds left comment on next page" $ do
        cursor <- runMock getAll givenComments <&> Just
          . fromRight undefined
          . Cursor.decode
          . fromJust
          . Page.nextCursor
        let
          getNext = getPostComments sndId sortCreated orderDesc cursor (Just 2)
          expectedTitles = [Comment.title thirdComment]
        page <- runMock getNext givenComments
        CommentDto.title <$> Page.content page `shouldBe` expectedTitles
        Page.hasNextPage page `shouldBe` False
        Page.nextCursor page `shouldBe` Nothing
        Page.pageSize page `shouldBe` 2

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

    context "When creating a reply" $ do
      let
        newComment = NewCommentDto
          { NewCommentDto.title = "Title"
          , NewCommentDto.content = "Content"
          }

      it "Finds the reply" $ do
        comment <- runMock
          (createCommentReply fstCommentId newComment
            >>= getComment . CommentIdDto.commentId
          ) givenComments
        CommentDto.title comment `shouldBe` NewCommentDto.title newComment
        CommentDto.content comment `shouldBe` NewCommentDto.content newComment
        CommentDto.authorId comment `shouldBe` idUser
        CommentDto.parentId comment `shouldBe` Just fstCommentId
        CommentDto.postId comment `shouldBe` fstId

      it "Throws error if replied comment does not exist" $ do
        runMock (createCommentReply (Id nil) newComment) givenComments
          `shouldThrow` serverError 404

    it "Finds all replies" $ do
      let
        getReplies = getCommentReplies
          sndCommentId
          defaultSort
          defaultOrder
          fromFirst
          defaultPageSize
      repliesInPost <- runMock (Page.content <$> getReplies) givenComments
      length repliesInPost `shouldBe` 2
      repliesInPost `shouldSatisfy`
        all ((== Just sndCommentId) . CommentDto.parentId)
