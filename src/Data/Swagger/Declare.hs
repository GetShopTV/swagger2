{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:      Data.Swagger.Declare
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Declare monad transformer and associated functions.
module Data.Swagger.Declare where

import Prelude ()
import Prelude.Compat

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont (ContT)
#if __GLASGOW_HASKELL__ >= 710
import Control.Monad.Except (ExceptT)
#endif
import Control.Monad.List (ListT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Identity
import Data.Monoid

-- | A declare monad transformer parametrized by:
--
--  * @d@ — the output to accumulate (declarations);
--
--  * @m@ — the inner monad.
--
-- This monad transformer is similar to both state and writer monad transformers.
-- Thus it can be seen as
--
--  * a restricted append-only version of a state monad transformer or
--
--  * a writer monad transformer with the extra ability to read all previous output.
newtype DeclareT d m a = DeclareT { runDeclareT :: d -> m (d, a) }
  deriving (Functor)

instance (Applicative m, Monad m, Monoid d) => Applicative (DeclareT d m) where
  pure x = DeclareT (\_ -> pure (mempty, x))
  DeclareT df <*> DeclareT dx = DeclareT $ \d -> do
    ~(d',  f) <- df d
    ~(d'', x) <- dx (d <> d')
    return (d' <> d'', f x)

instance (Applicative m, Monad m, Monoid d) => Monad (DeclareT d m) where
  return x = DeclareT (\_ -> pure (mempty, x))
  DeclareT dx >>= f = DeclareT $ \d -> do
    ~(d',  x) <- dx d
    ~(d'', y) <- runDeclareT (f x) (d <> d')
    return (d' <> d'', y)

instance Monoid d => MonadTrans (DeclareT d) where
  lift m = DeclareT (\_ -> (,) mempty `liftM` m)

-- |
-- Definitions of @declare@ and @look@ must satisfy the following laws:
--
-- [/monoid homomorphism (mempty)/]
--   @'declare' mempty == return ()@
--
-- [/monoid homomorphism (mappend)/]
--   @'declare' x >> 'declare' y == 'declare' (x <> y)@
--   for every @x@, @y@
--
-- [/@declare@-@look@/]
--   @'declare' x >> 'look' == 'fmap' (<> x) 'look' <* 'declare' x@
--   for every @x@
--
-- [/@look@ as left identity/]
--   @'look' >> m == m@
--   for every @m@
class (Applicative m, Monad m) => MonadDeclare d m | m -> d where
  -- | @'declare' x@ is an action that produces the output @x@.
  declare :: d -> m ()
  -- | @'look'@ is an action that returns all the output so far.
  look :: m d

instance (Applicative m, Monad m, Monoid d) => MonadDeclare d (DeclareT d m) where
  declare d = DeclareT (\_ -> return (d, ()))
  look = DeclareT (\d -> return (mempty, d))

-- | Lift a computation from the simple Declare monad.
liftDeclare :: MonadDeclare d m => Declare d a -> m a
liftDeclare da = do
  (d', a) <- looks (runDeclare da)
  declare d'
  pure a

-- | Retrieve a function of all the output so far.
looks :: MonadDeclare d m => (d -> a) -> m a
looks f = f <$> look

-- | Evaluate @'DeclareT' d m a@ computation,
-- ignoring new output @d@.
evalDeclareT :: Monad m => DeclareT d m a -> d -> m a
evalDeclareT (DeclareT f) d = snd `liftM` f d

-- | Execute @'DeclateT' d m a@ computation,
-- ignoring result and only producing new output @d@.
execDeclareT :: Monad m => DeclareT d m a -> d -> m d
execDeclareT (DeclareT f) d = fst `liftM` f d

-- | Evaluate @'DeclareT' d m a@ computation,
-- starting with empty output history.
undeclareT :: (Monad m, Monoid d) => DeclareT d m a -> m a
undeclareT = flip evalDeclareT mempty

-- | A declare monad parametrized by @d@ — the output to accumulate (declarations).
--
-- This monad is similar to both state and writer monads.
-- Thus it can be seen as
--
--  * a restricted append-only version of a state monad or
--
--  * a writer monad with the extra ability to read all previous output.
type Declare d = DeclareT d Identity

-- | Run @'Declare' d a@ computation with output history @d@,
-- producing result @a@ and new output @d@.
runDeclare :: Declare d a -> d -> (d, a)
runDeclare m = runIdentity . runDeclareT m

-- | Evaluate @'Declare' d a@ computation, ignoring output @d@.
evalDeclare :: Declare d a -> d -> a
evalDeclare m = runIdentity . evalDeclareT m

-- | Execute @'Declate' d a@ computation, ignoring result and only
-- producing output @d@.
execDeclare :: Declare d a -> d -> d
execDeclare m = runIdentity . execDeclareT m

-- | Evaluate @'DeclareT' d m a@ computation,
-- starting with empty output history.
undeclare :: Monoid d => Declare d a -> a
undeclare = runIdentity . undeclareT

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadDeclare d m => MonadDeclare d (ContT r m) where
  declare = lift . declare
  look = lift look

#if __GLASGOW_HASKELL__ >= 710
instance MonadDeclare d m => MonadDeclare d (ExceptT e m) where
  declare = lift . declare
  look = lift look
#endif

instance MonadDeclare d m => MonadDeclare d (IdentityT m) where
  declare = lift . declare
  look = lift look

instance MonadDeclare d m => MonadDeclare d (ListT m) where
  declare = lift . declare
  look = lift look

instance MonadDeclare d m => MonadDeclare d (MaybeT m) where
  declare = lift . declare
  look = lift look

instance MonadDeclare d m => MonadDeclare d (ReaderT r m) where
  declare = lift . declare
  look = lift look

instance (Monoid w, MonadDeclare d m) => MonadDeclare d (Lazy.WriterT w m) where
  declare = lift . declare
  look = lift look

instance (Monoid w, MonadDeclare d m) => MonadDeclare d (Strict.WriterT w m) where
  declare = lift . declare
  look = lift look
