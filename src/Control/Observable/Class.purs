module Control.Observable.Class where

import Prelude
import Data.Iterable (class Iterable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)


type Listeners a = {
  next ∷ a → Unit,
  error ∷ Error → Unit,
  complete ∷ Unit → Unit
}

newtype Subscription = Subscription { unsubscribe ∷ ∀ e. Unit → Eff e Unit }

-- | Purescript doesn't support polyvariadic functions, so `of` (`make`) takes a
-- | single argument instead . If more values need to be supplied, use
-- | `fromFoldable` and provide an array of values.
-- | In the spec, `from` can take an Observable or an Iterable. This is split
-- | into two functions `fromFoldable` and `fromObservable`. All Iterable
-- | structures are Foldable, so Foldable is used instead
class Observable o where
  subscribe ∷ ∀ a e. o a → Listeners a → Eff e Subscription
  make ∷ ∀ a e. a → Eff e (o a)
  fromIterable ∷ ∀ a e f. Iterable (f a) a ⇒ f a → Eff e (o a)
  fromObservable ∷ ∀ a e. o a → Eff e (o a)
