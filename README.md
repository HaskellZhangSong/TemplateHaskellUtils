# TemplateHaskellUtils
useful functions for you to write template Haskell code

some examples
-- | Apply a list of expression [(+), 1, 2] to (+) 1 2
     	   appExp :: [ExpQ] -> ExpQ

-- | Apply a type constructor, convert [a, b, c] to a b c
     	   appConT :: [TypeQ] -> TypeQ

-- | convert [a, b, c] to a -> b -> c
     	   curryType :: [TypeQ] -> TypeQ
