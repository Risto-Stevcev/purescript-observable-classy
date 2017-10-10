module Test.Control.Observable.Class where

import Control.Observable.Class

import Control.Coroutine (Consumer, Process, Producer, await, runProcess, ($$))
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Iterable (class Iterable)
import Prelude ( class Applicative, class Functor, class Semigroup, Unit, bind
               , discard, pure, unit, ($), (*), (<$>), (<>) )
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)


foreign import data Observable ∷ Type → Type
foreign import _of ∷ ∀ a e. a → Eff e (Observable a)
foreign import _subscribe ∷ ∀ a e. Observable a → Listeners a → Eff e Subscription
foreign import _fromIterable ∷ ∀ a e f. Iterable (f a) a ⇒ f a → Eff e (Observable a)
foreign import _fromObservable ∷ ∀ a e. Observable a → Eff e (Observable a)
foreign import _map ∷ ∀ a b. (a → b) → Observable a → Observable b

instance observableObservable ∷ Observable Observable where
  subscribe = _subscribe
  make = _of
  fromIterable = _fromIterable
  fromObservable = _fromObservable

instance functorObservable ∷ Functor Observable where
  map = _map

data ObservableEvent a = Next a | Error | Complete

observableProducer
  ∷ ∀ e a. Observable a
  → Producer (ObservableEvent a) (Aff (avar ∷ AVAR | e)) Unit
observableProducer observable = produce \emit → do
   _ ← subscribe observable {
     next:     \a → unsafePerformEff $ emit (Left $ Next a),
     error:    \_ → unsafePerformEff $ emit (Left Error),
     complete: \_ → unsafePerformEff $ do
       emit (Left Complete)
       emit (Right unit)
   }
   pure unit

observableConsumer
  ∷ ∀ a e f. Applicative f ⇒ Semigroup (f a) ⇒ STRef Unit (f a)
  → Consumer (ObservableEvent a) (Aff (st ∷ ST Unit | e)) Unit
observableConsumer ref = forever do
  e ← await
  lift $ liftEff $ case e of
    Next a   → do
      _ ← modifySTRef ref (_ <> pure a)
      pure ref
    Error      → pure ref
    Complete → pure ref

observableProcess
  ∷ ∀ a e f. Applicative f ⇒ Semigroup (f a) ⇒ STRef Unit (f a) → Observable a
  → Process (Aff (st ∷ ST Unit, avar ∷ AVAR | e)) Unit
observableProcess ref observable = observableProducer observable $$ observableConsumer ref


observableCollect
  ∷ ∀ a e. Observable a → Aff (st ∷ ST Unit, avar ∷ AVAR | e) (Array a)
observableCollect observable = do
  ref ← liftEff $ newSTRef []
  runProcess $ observableProcess ref observable
  liftEff $ readSTRef ref



main ∷ Eff (RunnerEffects (st ∷ ST Unit)) Unit
main = run [consoleReporter] do
  describe "purescript-observable-classy" do
    describe "fromIterable" do
      it "should create an observable from an iterable" do
        o ← liftEff $ fromIterable [1, 1, 2, 3, 5, 8]
        v ← observableCollect o
        v `shouldEqual` [1,1,2,3,5,8]

    describe "fromObservable" do
      it "should create an observable from another observable" do
        o ← liftEff do
          o' ← fromIterable [1, 1, 2, 3, 5, 8]
          fromObservable $ (_ * 2) <$> o'
        v ← observableCollect o
        v `shouldEqual` [2,2,4,6,10,16]

    describe "make" do
      it "should create an observable from a value" do
        o ← liftEff $ make "foo"
        v ← observableCollect o
        v `shouldEqual` ["foo"]
