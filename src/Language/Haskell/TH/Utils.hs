{-# LANGUAGE TemplateHaskell  #-}

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
       getCompositeType,getConName,
       seqTuple2, seqTuple3,seqTuple4, 
       rename, rename', rename'',
       nameToExp, printQ
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Data.List (foldl1,foldr1)
import Control.Monad
import Language.Haskell.TH.Syntax

-- | Pretty print spliced code
pprintQ :: Ppr a => Q a -> IO ()
pprintQ q = runQ q >>= putStrLn.pprint

-- | Print AST
printQ :: Show a => Q a -> IO ()
printQ q = runQ q >>= putStrLn.show

-- | sequence-like functons on tuples
seqTuple2 :: (Q a, Q b) -> Q (a, b) 
seqTuple2 (a,b) = liftM2 (,) a b

seqTuple3 :: (Q a, Q b, Q c) -> Q (a, b, c)
seqTuple3 (a,b,c) = liftM3 (,,) a b c

seqTuple4 :: (Q a, Q b, Q c, Q d) -> Q (a, b, c, d)
seqTuple4 (a,b,c,d) = liftM4 (,,,) a b c d

-- | Rename a 'Name'
rename' :: Name -> (String -> String) -> Name
rename' n f = mkName $ f $ nameBase n

rename'' :: Name -> (String -> String) -> Q Name
rename'' n f = do
          let nameStr = f $ nameBase n
          return $ mkName nameStr

rename :: Q Name -> (String -> String) -> Q Name
rename n f = do
         bn <- n
         let nameStr = f $ nameBase bn
         return $ mkName nameStr

{-|
> data Foo = Foo { foo :: Int }
> > $(nameToExp (++"1") 'foo)
> "foo1"       
-}

nameToExp :: (String -> String) -- ^ Function to change name. 
             -> Name 
             -> Q Exp 
nameToExp f = litE . stringL . f . nameBase 

-- | Makes a string literal expression from a constructor's name. 
conNameExp :: Con -> Q Exp 
conNameExp = litE . stringL . nameBase . getConName

-- | Apply a list of expression
-- | > [(+), 1, 2] to (+) 1 2
appExp :: [ExpQ] -> ExpQ
appExp = appsE

appExp' :: [Exp] -> Exp
appExp' = foldl1 AppE

-- | Apply a type constructor
-- | > convert [a, b, c] to a b c like 'appExp'

{-|
> > pprint $ appConT' (map ConT [''(,), ''Int , ''Bool])
> "GHC.Tuple.(,) GHC.Types.Int GHC.Types.Bool" --i.e. (Int,Bool)
-}

appConT :: [TypeQ] -> TypeQ
appConT = foldl1 appT

appConT' :: [Type] -> Type
appConT' = foldl1 AppT

-- | > convert [a, b, c] to a -> b -> c
{-|
> > pprint $ curryType' (map ConT [''Int , ''Int , ''Bool])
> "GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool"
-}
curryType :: [TypeQ] -> TypeQ
curryType  = foldr1 (\t1 -> appT (appT arrowT t1))

curryType' :: [Type] -> Type
curryType' = foldr1 (\t1 -> AppT (AppT ArrowT t1))

{-|
> > genBT' "a" 3
> ([PlainTV a1,PlainTV a2,PlainTV a3],[VarT a1,VarT a2,VarT a3])
-}

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
> > genPE' "a" 3
> ([VarP a1,VarP a2,VarP a3],[VarE a1,VarE a2,VarE a3])
-}

-- | Related patterns and expressions
genPE :: String -> Int -> Q ([PatQ],[ExpQ])
genPE name n = do 
           let ns = [name++ (show i) | i <- [1..n]]
           pat <- sequence $ map (return.varP.mkName) ns
           exp <- sequence $ map (return.varE.mkName) ns
           return (pat,exp)

genPE' :: String -> Int -> ([Pat], [Exp])
genPE' name n = let ns = [name++ (show i) | i <- [1..n]] 
                 in (map (VarP . mkName) ns,map (VarE . mkName) ns)

-- | Apply a list of kinds, like 'appConT'
appKinds :: [Kind] -> Kind
appKinds = foldr1 AppT

-- | > convert [k1,k2,k3] to k1 -> k2 -> k3,like 'curryType'
curryKind :: [Kind] -> Kind
curryKind =  curryType'

-- | Get name from constructors
getConName :: Con -> Name 
getConName (NormalC name _)  = name 
getConName (RecC name _)     = name 
getConName (InfixC _ name _) = name 
getConName (ForallC _ _ con) = getConName con 

-- | Get type Names recursively
getTypeNames :: Type -> [Name]
getTypeNames (ForallT tvbs cxt t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

-- | Get type var bind name
getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV  name  ) = name
getTVBName (KindedTV name _) = name

third (a,b,c) = c

-- | Get all names recursively from a constructor
getCompositeType :: Con -> [Name]
getCompositeType (NormalC n sts)        = concatMap getTypeNames (map snd sts)
getCompositeType (RecC    n vars)       = concatMap getTypeNames (map third vars)
getCompositeType (InfixC st1 n st2)     = concatMap getTypeNames [snd st1 , snd st2]
-- This could be a problem since it will lose info for context and type variables 
getCompositeType (ForallC tvbs cxt con) = getCompositeType con




