-- | Small build-time wrapper that makes long proof runs attributable.
--
-- Pantomime's solver transcript does not identify the splice that produced a
-- query.  Printing a marker on either side lets a timeout be mapped back to the
-- exact obligation without changing the proposition sent to the solver.
module LoggedPantomime
  ( pantomime,
  )
where

import Language.Haskell.TH.Syntax (Exp, Name, Q, nameBase, runIO)
import qualified Pantomime
import Prelude (String, pure, putStrLn, ($), (++))

pantomime :: Name -> Q Exp
pantomime property = do
  marker "BEGIN"
  result <- Pantomime.pantomime property
  marker "END"
  pure result
  where
    marker :: String -> Q ()
    marker phase =
      runIO $
        putStrLn ("PANTOMIME_" ++ phase ++ " " ++ nameBase property)
