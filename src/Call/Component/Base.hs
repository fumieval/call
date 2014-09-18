{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Call.Component.Base where

import Control.Comonad.Zero
import Control.Comonad
import Control.Monad.Trans.State

infix 3 .-
infix 3 .&

newtype Component e m = Component { runComponent :: forall x. e x -> m (x, Component e m) }

oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Component e m
oneshot m = go where
  go = Component $ \e -> m (fmap return e) >>= \a -> return (a, go)

newtype Control s (e :: * -> *) = Control Int

class Monad m => MonadObjective s m where
  type Base m :: * -> *
  (.-) :: Control s e -> e a -> m a
  invoke :: Component e (Base m) -> m (Control s e)

class Stateful s f where
  get_ :: f s
  put_ :: s -> f ()

(.&) :: (MonadObjective k m, Stateful s e) => Control k e -> StateT s m a -> m a
c .& m = do
  s <- c .- get_
  (a, s') <- runStateT m s
  c .- put_ s'
  return a

data AccessT s f a = Get (s -> a) | Put s a | LiftAccessT (f a)

instance Stateful s (AccessT s f) where
  get_ = Get id
  put_ s = Put s ()

variable :: Monad m => s -> Component (AccessT s Zero) m
variable s = Component $ \x -> case x of
  Get cont -> return (cont s, variable s)
  Put s' cont -> return (cont, variable s')
  LiftAccessT e -> return (extract e, variable s)