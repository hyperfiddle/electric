-- https://hackage.haskell.org/package/transformers-0.2.2.0/docs/src/Data-Functor-Identity.html#Identity

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

class Functor f where
  fmap :: (a -> b) -> f a -> f b -- "substitution"

  (<$) :: a -> f b -> f a
  (<$) =  fmap . const

class Functor f => Applicative f where
  pure :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 id

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x = (<*>) (fmap f x)

  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const

class Applicative m => Monad m where
  join :: m m a -> m a -- "renormalization"
  return :: a -> m a
  (>>=) :: (a -> m b) -> m a -> m b
  m >>= f = join (fmap f m)

-- In practice, you can just view a Free f a as many layers of f wrapped around values of type a,
-- where (>>=) performs substitution and grafts new layers of f in for each of the free variables.
-- This can be very useful for modeling domain specific languages, trees, or other constructs.
-- Given data Empty a, Free Empty is isomorphic to the Identity monad. (there are no effects)
-- https://stackoverflow.com/questions/14641864/what-monads-can-be-expressed-as-free-over-some-functor
-- https://hackage.haskell.org/package/free-5.1.6/docs/Control-Monad-Free.html
-- You use free monads whenever you need an abstract syntax tree.
--  The base functor of the free monad is the shape of each step of the syntax tree.
-- https://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
data Free f a = Pure a | Free (f (Free f a))
data Cofree f a = a :< f (Cofree f a) -- The f-branching stream comonad, aka the cofree comonad for a Functor f.


data F a = One a | Two a a | Two2 a a | Three Int a a a
-- Then Free F a is the type of trees whose leaves have type a and whose nodes are tagged with One, Two, Two2 and Three

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance Functor f => Apply (Free f) where
  Pure a  <.> Pure b = Pure (a b)
  Pure a  <.> Free fb = Free $ fmap a <$> fb
  Free fa <.> b = Free $ (<.> b) <$> fa

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)


data Terminal a = GetLine (String -> a) | PrintLine String a deriving Functor

type TerminalM = Free Terminal


traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)

-- Comonad     https://hackage.haskell.org/package/comonad-5.0.8/docs/Control-Comonad.html
-- comonad can extract but not insert. "contextual computation"
class Functor w => Comonad w where
  extract :: w a -> a -- dual of return

  duplicate :: w a -> w (w a) -- dual of join
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b -- dual of bind
  extend f = fmap f . duplicate

unsequence :: (Comonad w, Monad m) => w [a] -> [m a]
unsequence = map return . extract

-- Stream
data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)

-- https://hackage.haskell.org/package/free-5.1.6/docs/Control-Comonad-Cofree.html
-- The f-branching stream comonad, aka the cofree comonad for a Functor f.
data Cofree f a = a :< f (Cofree f a)

instance Functor f => Functor (Cofree f) where
  fmap f (a :< as) = f a :< fmap (fmap f) as
  b <$ (_ :< as) = b :< fmap (b <$) as

instance Functor f => Comonad (Cofree f) where
  extend f w = f w :< fmap (extend f) (unwrap w)
  duplicate w = w :< fmap duplicate (unwrap w)
  extract (a :< _) = a






newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f m = Identity (f (runIdentity m))

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return a = Identity a
  m >>= k  = k (runIdentity m)

instance MonadFix Identity where
  mfix f = Identity (fix (runIdentity . f))




