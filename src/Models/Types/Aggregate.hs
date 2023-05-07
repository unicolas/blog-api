module Models.Types.Aggregate (Aggregate(..), aggregate, aggregateMaybe) where

import Control.Applicative (liftA2)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import Models.Types.Entity (Entity(..))

data Aggregate a b = Aggregate (Entity a) b
  deriving (Show, Eq)

instance
  ( Postgres.FromRow a
  , Postgres.FromRow b
  ) => Postgres.FromRow (Aggregate a b) where
  fromRow :: Postgres.RowParser (Aggregate a b)
  fromRow = Aggregate <$> Postgres.fromRow <*> Postgres.fromRow

-- | Aggregates from a tuple of lifted values
aggregate :: Applicative f => (f (Entity a), f b) -> f (Aggregate a b)
aggregate = uncurry (liftA2 Aggregate)

-- | Optionally aggregates from a tuple of lifted values
aggregateMaybe :: Applicative f
  => (f (Maybe (Entity a)), f b) -> f (Maybe (Aggregate a b))
aggregateMaybe = uncurry $ liftA2 (\maybeA b -> (`Aggregate` b) <$> maybeA)
