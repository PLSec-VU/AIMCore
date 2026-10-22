-- | Small build-time wrapper that makes long proof runs attributable.
--
-- Pantomime's solver transcript does not identify the splice that produced a
-- query.  Printing a marker on either side lets a timeout be mapped back to the
-- exact obligation without changing the proposition sent to the solver.
module Proof.SMT.Logged
  ( pantomime,
  )
where

import Language.Haskell.TH.Syntax (Exp, Name, Q, nameBase, runIO)
-- import qualified SymFC
import Prelude (String, pure, putStrLn, ($), (++), undefined)

pantomime :: Name -> Q Exp
pantomime property = do
  marker "BEGIN"
  -- TODO: This should be changed!!!
  result <- pure undefined
  marker "END"
  pure result
  where
    marker :: String -> Q ()
    marker phase =
      runIO $
        putStrLn ("PANTOMIME_" ++ phase ++ " " ++ nameBase property)
