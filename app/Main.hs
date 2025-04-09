module Main (main) where

-- import Control.Monad.IO.Class
-- import Instruction
-- import Pipe (pipe)
-- import Pretty
-- import Regfile
-- import Types (Word)
-- import Prelude hiding (Word, init)

main :: IO ()
main = pure ()

-- main :: IO ()
-- main = do
--  -- _ <- mealyM pipe init program
--  pprint initRF
--  pure ()
--
-- mealy :: (s -> i -> (s, o)) -> s -> [i] -> [o]
-- mealy _ _ [] = []
-- mealy step s (i:is) = do
--  let (s', o) = step s i
--  o : mealy step s' is
--
-- mealyM :: MonadIO m => Pretty s => Pretty o => (s -> i -> (s, o)) -> s -> [i] -> m [o]
-- mealyM _ _ [] = pure []
-- mealyM step s (i:is) = do
--  let (s', o) = step s i
--  pprint s'
--  pprint o
--  os <- mealyM step s' is
--  pure $ o : os
--
-- program :: [Word]
-- program = encode <$>
--  -- %r1 := %r0 + 20
--  [ IType (Arith ADD) 1 0 20
--  -- %r1 := %r0 + 20
--  , IType (Arith ADD) 1 0 20
--  -- %r1 := %r0 + 20
--  , IType (Arith ADD) 1 0 20
--  -- %r1 := %r0 + 20
--  , IType (Arith ADD) 1 0 20
--  -- %r1 := %r0 + 20
--  , IType (Arith ADD) 1 0 20
--  -- %r1 := %r0 + 20
--  , IType (Arith ADD) 1 0 20
--  -- %r1 := %r0 + 20
--  , IType (Arith ADD) 1 0 20
--  ]
