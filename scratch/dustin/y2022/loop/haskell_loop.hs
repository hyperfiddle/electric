

-- https://en.wikipedia.org/wiki/Fixed-point_combinator

-- Y = λg. (λx. g (x x)) (λx. g (x x))
-- const Y = g => (x => g(x(x)))(x => g(x(x)))
-- The fact that Y(f) = f(Y(f)) makes Y a fixed-point combinator.

-- The Z ‘strict’ fixed point combinator
-- Z = λg.(λx.g(λv.xxv))(λx.g(λv.xxv))
-- const Z = g => (x => g(v => x(x)(v)))(x => g(v => x(x)(v)))
-- Z(f) = f(v => Z(f)(v))

-- Many ways to define Y
fix f = f (fix f)               -- Y combinator
fix f = let x = f x in x        -- mutual recursion
fix f = f (\x -> fix f x)       -- Church encoding
fix f = mu X. f X               -- Fixpoint operator
fix f = (\x -> f (\y -> (x x) y))
        (\x -> f (\y -> (x x) y))   -- Z combinator

newtype Fix f = Fix (f (Fix f)) -- Recursive newtype
fix :: Functor f => f (Fix f) -> Fix f
fix = Fix


counter :: ArrowCircuit a => a Bool Int
counter = proc reset -> do
        rec     output <- returnA -< if reset then 0 else next
                next <- delay 0 -< output+1
        returnA -< output


import Control.Arrow
import Control.Arrow.Loop

fibonacci :: ArrowLoop a => a () Int
fibonacci = proc () -> do
rec a <- arr (\(_,b) -> b) -< (a, b)
b <- arr (\(_,a) -> a+b) -< (a, b)
returnA -< a


-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
{-# LANGUAGE RecursiveDo #-}
justOnes = mdo { xs <- Just (1:xs)
               ; return (map negate xs) }

-- http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Arrow.html#t:ArrowLoop

class Arrow a => ArrowLoop a where
  loop :: a (b,d) (c,d) -> a b c

instance ArrowLoop (->) where
  loop f b = let (c,d) = f (b,d) in c

-- Prelude> loop f b = let (c,d) = f (b,d) in c
-- Prelude> f (b,d) = (drop (d-2) b, length b)
-- Prelude> loop f "Hello World"
-- "ld"



-- https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-Fix.html
--
-- The fixed point of a monadic computation.
-- @'mfix' f@ executes the action @f@ only once, with the eventual
-- output fed back as the input.  Hence @f@ should not be strict,
-- for then @'mfix' f@ would diverge.

class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a

-- | Beware that for many monads (those for which the '>>=' operation
-- is strict) this instance will /not/ satisfy the right-tightening law
-- required by the 'ArrowLoop' class.
--
-- @since 2.01
instance MonadFix m => ArrowLoop (Kleisli m) where
    loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
      where f' x y = f (x, snd y)


instance MonadFix Signal => ArrowLoop Signal where
    loop f = Signal (liftM fst . mfix . f')
      where f' x y = f (x, snd y)

-- loop is recursive iteration where the effect is run once
-- mfix is general recursion where the effect is run once


class ArrowLoop a => ArrowCircuit a where
    -- | A delay component.
    delay ::
        b        -- ^ the value to return initially.
        -> a b b -- ^ an arrow that propagates its input with a one-tick delay.

-- StreamArrow
-- https://hackage.haskell.org/package/arrows-0.3
-- https://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Transformer-Stream.html#t:StreamArrow

newtype StreamArrow a b c = StreamArrow (a (Stream b) (Stream c))

instance ArrowLoop a => ArrowCircuit (StreamArrow a) where
    delay x = StreamArrow (arr (Cons x))

-- I don't think this is what i'm looking for
instance MonadPlus m => ArrowPlus (Kleisli m) where
    Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)