module Pretty
  ( Pretty (..)
  , Doc
  , P.render
  , pprint

  -- , (P.<:>)
  , (<+>)
  -- , (P.$:$)
  , (P.$+$)

  , P.hcat
  , P.hsep
  , P.vcat
  , P.sep
  , P.cat
  , P.fsep
  , P.fcat
  , P.nest
  , P.hang
  , P.punctuate

  , P.parens
  , P.brackets
  , P.braces
  , P.quotes
  , P.doubleQuotes
  ) where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc)
import Control.Monad.IO.Class
import Clash.Prelude (BitVector, Unsigned, Signed, KnownNat)

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty = id

instance Pretty Char where
  pretty = P.char

instance Pretty [Char] where
  pretty = P.text

instance Pretty Int where
  pretty = P.int

instance Pretty Integer where
  pretty = P.integer

instance Pretty Float where
  pretty = P.float

instance Pretty Double where
  pretty = P.double

instance Pretty Rational where
  pretty = P.rational

instance KnownNat n => Pretty (BitVector n) where
  pretty = pretty . show

instance Pretty (Unsigned n) where
  pretty = pretty . show

instance Pretty (Signed n) where
  pretty = pretty . show

pprint :: MonadIO m => Pretty a => a -> m ()
pprint = liftIO . putStrLn . P.render . pretty

-- (<:>) :: Pretty a => Pretty b => a -> b -> Doc
-- (<:>) x y = pretty x P.<> pretty y
-- (<:>) :: Doc -> Doc -> Doc
-- (<:>) = (P.<>)

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) x y = x P.<+> y

-- ($:$) :: Pretty a => Pretty b => a -> b -> Doc
-- ($:$) x y = pretty x P.$$ pretty y

-- ($+$) :: Pretty a => Pretty b => a -> b -> Doc
-- ($+$) x y = pretty x P.$+$ pretty y

-- parens :: Pretty a => a -> Doc
-- parens = P.parens . pretty

-- brackets :: Pretty a => a -> Doc
-- brackets = P.brackets . pretty

-- braces :: Pretty a => a -> Doc
-- braces = P.braces . pretty

-- quotes :: Pretty a => a -> Doc
-- quotes = P.quotes . pretty

-- doubleQuotes :: Pretty a => a -> Doc
-- doubleQuotes = P.doubleQuotes . pretty

