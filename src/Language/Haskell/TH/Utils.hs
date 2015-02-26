{-# LANGUAGE TemplateHaskell , DeriveGeneric,StandaloneDeriving, TypeFamilies,UndecidableInstances, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.Utils
-- Copyright   :  None
-- License     :  MIT (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Haskell.Zhang.Song@hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Some auxiliary functions that might be needed when using template Haskell
--
-----------------------------------------------------------------------------

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
       seqTup2, seqTup3,seqTup4, 
       rename, rename', rename'',
       nameToExp, 
       printQ, pprintQ, printiQ, printi,
       -- | exported GenericPretty package
       module Text.PrettyPrint.GenericPretty
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Lib hiding (Role)
import Data.List (foldl1,foldr1)
import Control.Monad
import Language.Haskell.TH.Syntax
import Text.PrettyPrint.GenericPretty
import GHC.Generics hiding (Fixity)
import GHC.Word
import GHC.Types

-- sorry for the following messy, I will write a library to generate empty instances automatically
instance Out Name
instance Out Type
instance Out TyVarBndr
instance Out TyLit
instance Out Exp
instance Out Lit
instance Out Pat
instance Out Dec
instance Out Clause
instance Out FunDep
instance Out Foreign
instance Out Fixity
instance Out Pragma
instance Out FamFlavour
instance Out TySynEqn
instance Out Role
instance Out Loc
instance Out Info
instance Out Module
instance Out ModuleInfo
instance Out Strict
instance Out Callconv
instance Out Safety
instance Out Stmt
instance Out Range
instance (Generic a,Out a) => Out (TExp a)
instance Out FixityDirection
instance Out Match
instance Out Guard
instance Out Body
instance Out ModName
instance Out NameFlavour
instance Out Con
instance Out Inline
instance Out RuleBndr
instance Out RuleMatch
instance Out Pred
instance Out Phases
instance Out PkgName
instance Out OccName
instance Out AnnTarget
instance Out Word8

-- sorry for the messy, I will also write a library to do this automatically
deriving instance Generic FixityDirection
deriving instance Generic Inline
deriving instance Generic RuleBndr
deriving instance Generic Match
deriving instance Generic Name
deriving instance Generic RuleMatch
deriving instance Generic Pred
deriving instance Generic Phases
deriving instance Generic Con
deriving instance Generic Module
deriving instance Generic AnnTarget
deriving instance Generic Type
deriving instance Generic TyVarBndr
deriving instance Generic TyLit
deriving instance Generic Exp
deriving instance Generic Lit
deriving instance Generic Pat
deriving instance Generic Dec
deriving instance Generic Clause
deriving instance Generic FunDep
deriving instance Generic Foreign
deriving instance Generic Fixity
deriving instance Generic Pragma
deriving instance Generic FamFlavour
deriving instance Generic TySynEqn
deriving instance Generic Role
deriving instance Generic Loc
deriving instance Generic Info
deriving instance Generic ModuleInfo
deriving instance Generic Strict
deriving instance Generic Callconv 
deriving instance Generic Safety
deriving instance Generic Stmt
deriving instance Generic Range 
deriving instance Generic a => Generic (TExp a)
deriving instance Generic Guard 
deriving instance Generic Body
deriving instance Generic ModName
deriving instance Generic PkgName
deriving instance Generic OccName

data C_Word8
data D_Word8

instance Datatype D_Word8 where
  datatypeName _ = "Word"
  moduleName   _ = "GHC.Word"
instance Constructor C_Word8 where
  conName _ = ""

deriving instance Generic NameSpace
instance Out NameSpace 

instance Generic Word8 where
  type Rep Word8 = D1 D_Word8 (C1 C_Word8 (S1 NoSelector (Rec0 Word8)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x

-- This is bacause GHC 7.8 does not support unboxed value to be made into Generic
instance Generic NameFlavour where
  type Rep NameFlavour = (Rep NameFlavour')
  from (NameS) = from NameS'
  from (NameQ m) = from (NameQ' m)
  from (NameU i) = from (NameU' (I# i))
  from (NameL i) = from (NameL' (I# i))
  from (NameG ns p m) = from (NameG' ns p m)
  to M1 {unM1 = L1 (L1 (M1 {unM1 = U1}))} = NameS
  to M1 {unM1 = L1 (R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = ModName mn }}}))} =  NameQ (ModName mn)
  to M1 {unM1 = R1 (L1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = (I# n)}}}))} = NameU n
  to M1 {unM1 = R1 (R1 (L1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = (I# n)}}})))} = NameL n
  to M1 {unM1 = R1 (R1 (R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = ns}} :*: (M1 {unM1 = K1 {unK1 = PkgName p}} :*: M1 {unM1 = K1 {unK1 = ModName m}})})))} = NameG ns (PkgName p) (ModName m)

data NameFlavour' = NameS' 
                  | NameQ' ModName 
                  | NameU' Int 
                  | NameL' Int 
                  | NameG' NameSpace PkgName ModName
                    deriving (Generic,Show)

deriving instance Show NameSpace

-- | Pretty print spliced code
pprintQ :: Ppr a => Q a -> IO ()
pprintQ q = runQ q >>= putStrLn.pprint

-- | Print AST with indentions. There are also other exported functions from genericpretty library. See 'Out'.
{-|
> runQ [| (1+1) * 5|] >>= pp -- or use
> printiQ  [| (1+1) * 5|]
> InfixE (Just InfixE (Just LitE (IntegerL 1))
>                   (VarE (Name (OccName "+")
>                                (NameG' VarName
>                                        (PkgName "base")
>                                        (ModName "GHC.Num"))))
>                    (Just LitE (IntegerL 1)))
>       (VarE (Name (OccName "*")
>                   (NameG' VarName
>                           (PkgName "base")
>                           (ModName "GHC.Num"))))
>       (Just LitE (IntegerL 5))
-}
printiQ :: Out a => Q a -> IO ()
printiQ q = runQ q >>= pp

-- | Print AST with identions
printi :: Out a => a -> IO ()
printi a = pp a

-- | Print AST of the code.
printQ :: Show a => Q a -> IO ()
printQ q = runQ q >>= putStrLn.show

-- | sequence-like functons on tuples
seqTup2 :: (Q a, Q b) -> Q (a, b) 
seqTup2 (a,b) = liftM2 (,) a b

seqTup3 :: (Q a, Q b, Q c) -> Q (a, b, c)
seqTup3 (a,b,c) = liftM3 (,,) a b c

seqTup4 :: (Q a, Q b, Q c, Q d) -> Q (a, b, c, d)
seqTup4 (a,b,c,d) = liftM4 (,,,) a b c d

-- | Rename a 'Name'
rename :: Q Name -> (String -> String) -> Q Name
rename n f = do
         bn <- n
         let nameStr = f $ nameBase bn
         return $ mkName nameStr

rename' :: Name -> (String -> String) -> Name
rename' n f = mkName $ f $ nameBase n

rename'' :: Name -> (String -> String) -> Q Name
rename'' n f = do
          let nameStr = f $ nameBase n
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
{-|
> [(+), 1, 2] to (+) 1 2
> appExp' [VarE '(+) , (LitE (IntegerL 1)), (LitE (IntegerL 2))]
> >AppE (AppE (VarE GHC.Num.+) (LitE (IntegerL 1))) (LitE (IntegerL 2))
-}

appExp :: [ExpQ] -> ExpQ
appExp = appsE

appExp' :: [Exp] -> Exp
appExp' = foldl1 AppE

-- | Apply a type constructor like 'appExp'

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

-- | Generate a list of type Bind and Type without Q
genBT' :: String -> Int -> ([TyVarBndr], [Type])
genBT' name n = let ns = [name++ (show i) | i <- [1..n]] 
                    in (map (plainTV.mkName) ns, map (VarT . mkName) ns)

{-|
> > genPE' "a" 3
> ([VarP a1,VarP a2,VarP a3],[VarE a1,VarE a2,VarE a3])
-}

-- | Generate related patterns and expressions
genPE :: String -> Int -> Q ([PatQ],[ExpQ])
genPE name n = do 
           let ns = [name++ (show i) | i <- [1..n]]
           pat <- sequence $ map (return.varP.mkName) ns
           exp <- sequence $ map (return.varE.mkName) ns
           return (pat,exp)

-- | Generate related patterns and expressions without Q
genPE' :: String -> Int -> ([Pat], [Exp])
genPE' name n = let ns = [name++ (show i) | i <- [1..n]] 
                 in (map (VarP . mkName) ns,map (VarE . mkName) ns)

-- | Apply a list of kinds, like 'appConT'
appKinds :: [Kind] -> Kind
appKinds = foldr1 AppT

-- | Like 'curryType' but on kind level
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

