{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenSym where

import Data.Monoid
import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as Map

type Sym = String

newtype GenSym w a = GenSym (RWS (Map Char Sym) w [Sym] a)
  deriving (Functor,Monad)

runGenSym :: [Sym] -> GenSym w a -> (a,[Sym],w)
runGenSym syms (GenSym m) = runRWS m Map.empty syms

genSym :: Monoid w => GenSym w Sym
genSym = GenSym $ do
  (x:xs) <- get  -- will fail if supply is empty!
  put xs
  return x

writeRaw :: Monoid w => w -> GenSym w ()
writeRaw = GenSym . tell

writeLine :: String -> GenSym String ()
writeLine s = do
  s' <- replaceVars s
  writeRaw (s' ++ "\n")

fetchVar :: Monoid w => Char -> GenSym w (Maybe Sym)
fetchVar = GenSym . asks . Map.lookup

bindVarsIn :: Monoid w => [(Char,Sym)] -> GenSym w a -> GenSym w a
bindVarsIn vs (GenSym m) = GenSym $ local (bind vs) m
  where bind = (flip Map.union) . Map.fromList

replaceVars :: Monoid w => String -> GenSym w String
replaceVars "" = return ""
replaceVars ('$' : '{' : x : '}' : xs) = do
  mbSym <- fetchVar x
  case mbSym of
    Just sym -> do
      xs' <- replaceVars xs
      return (sym ++ xs')
    Nothing -> do
      sym <- genSym
      bindVarsIn [(x,sym)] $ do
        xs' <- replaceVars xs
        return (sym ++ xs')
replaceVars (x:xs) = do
  xs' <- replaceVars xs
  return (x:xs')
