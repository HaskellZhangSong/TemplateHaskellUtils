# TemplateHaskellUtils
useful functions for you to write template Haskell code

some examples
** Prettyprint

    > runQ [| (1+1) * 5|] >>= pp -- or use
    > printiQ  [| (1+1) * 5|]
    InfixE (Just InfixE (Just LitE (IntegerL 1))
                      (VarE (Name (OccName "+")
                                   (NameG' VarName
                                           (PkgName "base")
                                           (ModName "GHC.Num"))))
                       (Just LitE (IntegerL 1)))
          (VarE (Name (OccName "*")
                      (NameG' VarName
                              (PkgName "base")
                              (ModName "GHC.Num"))))
          (Just LitE (IntegerL 5))

** Other helper functions

    -- | Apply a list of expression [(+), 1, 2] to (+) 1 2
    appExp :: [ExpQ] -> ExpQ

    -- | Apply a type constructor, convert [a, b, c] to a b c
    appConT :: [TypeQ] -> TypeQ

    -- | convert [a, b, c] to a -> b -> c
    curryType :: [TypeQ] -> TypeQ

