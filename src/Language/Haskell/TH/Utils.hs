module Language.Haskell.TH.Utils(
       appExp,
       appConT,
       curryType,
       genBT,
       genPE,
       appKinds,
       curryKind,
       getTypeNames,
       getTVBName ,
       getCompositeType
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Data.List (foldl1,foldr1)

-- | Apply a list of expression [(+), 1, 2] to (+) 1 2
appExp :: [ExpQ] -> ExpQ
appExp = appsE

-- | Apply a type constructor, convert [a, b, c] to a b c
appConT :: [TypeQ] -> TypeQ
appConT = foldl1 appT

-- | convert [a, b, c] to a -> b -> c
curryType :: [TypeQ] -> TypeQ
curryType  = foldr1 (\t1 -> appT (appT arrowT t1))

-- | Bind and Type
genBT :: String -> Int -> Q ([TyVarBndr], [TypeQ])
genBT name n = do
           let ns = [name++ (show i) | i <- [1..n]]
           tvb <- sequence $ map (return.plainTV.mkName) ns
           typ <- sequence $ map (return.varT.mkName) ns
           return (tvb,typ)

-- | Pattern and expression
genPE :: String -> Int -> Q ([PatQ],[ExpQ])
genPE name n = do 
           let ns = [name++ (show i) | i <- [1..n]]
           pat <- sequence $ map (return.varP.mkName) ns
           exp <- sequence $ map (return.varE.mkName) ns
           return (pat,exp)

-- | Apply a list of kinds
appKinds :: [Kind] -> Kind
appKinds = foldr1 AppT

-- | [k1,k2,k3] to k1 -> k2 -> k3
curryKind :: [Kind] -> Kind
curryKind =  foldr1 (\t1 -> appK (appK arrowK t1))

-- | get type Names recursively
getTypeNames :: Type -> [Name]
getTypeNames (ForallT tvbs cxt t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

-- | get type var bind name
getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV  name  ) = name
getTVBName (KindedTV name _) = name


third (a,b,c) = c

-- | get all names recursively from a constructor
getCompositeType :: Con -> [Name]
getCompositeType (NormalC n sts)        = concatMap getTypeNames (map snd sts)
getCompositeType (RecC    n vars)       = concatMap getTypeNames (map third vars)
getCompositeType (InfixC st1 n st2)     = concatMap getTypeNames [snd st1 , snd st2]
 --v this could be a problem since it will lose info for context and type variables 
getCompositeType (ForallC tvbs cxt con) = getCompositeType con

{-
{- | 
This function is used to check whether a type with typeclass constraint is an instance of a certain typeclass.
It is built since isInstance will not work for it.

>   test = do
        t <-  [t| forall a. Eq a => [a] |]
        isInstance ''Eq t
>   > runQ test
>   > False

>   t = ListT `AppT` VarT (mkName "a")
>   t' = ForallT [PlainTV (mkName "a")] [ClassP ''Show [VarT (mkName "a")]] t
>   > runTest = existentialTypeContainsClass ''Show t'
>   True

see http://stackoverflow.com/questions/20136559/how-to-check-whether-instance-exists-for-a-polymorphic-type-with-template-haskel
|-}

existentialTypeContainsClass :: Name -> Type -> Q Bool
existentialTypeContainsClass clss (ForallT _ cxt t) = return $ or $ map (boundByPred clss) cxt
existentialTypeContainsClass clas t = isInstance clas [t]

boundByPred :: Name -> Pred -> Bool
boundByPred _ (EqualP _ _)    = False
boundByPred c (ClassP clss _) = c == clss

-}
