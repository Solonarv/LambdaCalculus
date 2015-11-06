module UntypedTemplateBase where

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import qualified Untyped as U

 -- This should really be in Language.Haskell.TH, but it isn't. Oh well.
 -- for TH types f, t corresponding to types F, T
 -- f |-> is a TH type corresponding to the type F -> T
(|->) :: Type -> Type -> Type
f |-> t = AppT (AppT ArrowT f) t

ftype :: Integer -> Type
ftype n = ftype' n |-> ConT ''U.U
  where
    ftype' 0 = ConT ''U.U
    ftype' n = ConT ''U.U |-> ftype' (n-1)

mkLiftL :: Integer -> DecsQ
mkLiftL 1 = [d|
                liftL1 :: (U.U -> U.U) -> U.U
                liftL1 = U.U |]
mkLiftL n = let liftLn = mkName $ "liftL" ++ show n
                liftLp = mkName $ "liftL" ++ show (n-1)
                nameF = mkName "f"
                uCon = ConE 'U.U
                dot = VarE '(.)
            in return [SigD liftLn (ftype n),
                       FunD liftLn [Clause [VarP nameF]
                                           (NormalB $
                                                AppE uCon (InfixE (Just (VarE liftLp))
                                                                  dot
                                                                  (Just (VarE nameF)))) []]]

mkLiftLs :: Integer -> Q [Dec]
mkLiftLs n = fmap concat $ sequence $ map mkLiftL [1..n]