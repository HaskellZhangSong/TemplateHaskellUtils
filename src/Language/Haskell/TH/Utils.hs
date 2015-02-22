module Language.Haskell.TH.Utils(
       appExp, appExp',
       appConT, appConT',
       curryType, curryType',
       genBT, genBT',
       genPE, genPE',
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

-- | Apply a list of expression, [(+), 1, 2] to (+) 1 2
appExp :: [ExpQ] -> ExpQ
appExp = appsE

appExp' :: [Exp] -> Exp
appExp' = foldl1 AppE

-- | Apply a type constructor, convert [a, b, c] to a b c like 'appExp'
appConT :: [TypeQ] -> TypeQ
appConT = foldl1 appT

appConT' :: [Type] -> Type
appConT' = foldl1 AppT

{-|
> > pprint $ appConT' (map ConT [''(,), ''Int , ''Bool])
> "GHC.Tuple.(,) GHC.Types.Int GHC.Types.Bool"
|-}

-- | convert [a, b, c] to a -> b -> c
curryType :: [TypeQ] -> TypeQ
curryType  = foldr1 (\t1 -> appT (appT arrowT t1))

curryType' :: [Type] -> Type
curryType' = foldr1 (\t1 -> AppT (AppT ArrowT t1))

{-|
> > pprint $ curryType' (map ConT [''Int , ''Int , ''Bool])
> "GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool"
|-}

-- | Generate a list of type Bind and Type
genBT :: String -> Int -> Q ([TyVarBndr], [TypeQ])
genBT name n = do
           let ns = [name++ (show i) | i <- [1..n]]
           tvb <- sequence $ map (return.plainTV.mkName) ns
           typ <- sequence $ map (return.varT.mkName) ns
           return (tvb,typ)

genBT' :: String -> Int -> ([TyVarBndr], [Type])
genBT' name n = let ns = [name++ (show i) | i <- [1..n]] 
                    in (map (plainTV.mkName) ns, map (VarT . mkName) ns)

{-|
> > genBT' "a" 3
> ([PlainTV a1,PlainTV a2,PlainTV a3],[VarT a1,VarT a2,VarT a3])
|-}
       
-- | Pattern and expression
genPE :: String -> Int -> Q ([PatQ],[ExpQ])
genPE name n = do 
           let ns = [name++ (show i) | i <- [1..n]]
           pat <- sequence $ map (return.varP.mkName) ns
           exp <- sequence $ map (return.varE.mkName) ns
           return (pat,exp)

genPE' :: String -> Int -> ([Pat], [Exp])
genPE' name n = let ns = [name++ (show i) | i <- [1..n]] 
                 in (map (VarP . mkName) ns,map (VarE . mkName) ns)
{-|
> > genPE' "a" 3
> ([VarP a1,VarP a2,VarP a3],[VarE a1,VarE a2,VarE a3])
|-}
-- | Apply a list of kinds
appKinds :: [Kind] -> Kind
appKinds = foldr1 AppT

-- | convert [k1,k2,k3] to k1 -> k2 -> k3,like 'curryType'
curryKind :: [Kind] -> Kind
curryKind =  curryType'

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
-- This could be a problem since it will lose info for context and type variables 
getCompositeType (ForallC tvbs cxt con) = getCompositeType con

