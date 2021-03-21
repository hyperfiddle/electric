-- This can be defined as an inductive datatype Free f for a given functor f.

data Free f a where
  Var :: a → Free f a
  Con :: f (Free f a) → Free f a

data Free f Nondet where
  Var :: Nondet → Free f Nondet
  Con :: f (Free f Nondet) → Free f Nondet

data Nondet k where                         -- (Or 1 2)
  Or :: k → k → Nondet k

instance Functor Nondet where
  fmap f (Or x y) = Or (f x) (f y)          -- fmap inc (Or 1 2) => (Or 2 3)

coin :: Free Nondet Bool
coin = Con (Or (Var True) (Var False))      -- '(Or 'True 'False)

foo :: Free Nondet Bool
foo = Con (Or (Con (Or (Var True)           -- '(Or '(Or 'True 'False) 'False)
                       (Var False)))
              (Var False))

-- (eval foo)

type NondetProgram = Free Nondet Bool


data Nondet k where
  Or :: k → k → Nondet k                    -- e.g. (Or 1 2)

foo :: NondetProgram
foo = Con (Or (Con (Or (Var True)           -- '(Or '(Or 'True 'False) 'False)
                       (Var False)))
              (Var False))

import Control.Monad.Free {liftF}

or :: Bool Bool -> NondetProgram
or a b = liftF $ Or a b



-- '(Or '(Or 'True 'False) 'False)

main :: IO ()
main = do { x1 <- or True False;
            x2 <- or x1 False;
            x2 }

-- (eval {'or ...} '<-(or <-(or true false) false) )


ast :: Free Nondet Int                      -- (Or 1 2)
ast = Con (Or (Var 1) (Var 2))              -- Con $ fmap Var (Or 1 2)



; inductive data type
data List a  = Head a | List a