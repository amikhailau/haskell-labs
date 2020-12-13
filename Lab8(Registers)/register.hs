module AdvancedRegisterCalc
( RegisterState
, runCmds
, load
, write
, AdvancedRegisterCalc.read
, unop_cond
, unop
, negt
, sign
, inc
, dec
, binop_cond
, binop
, add
, sub
, mul
, sdiv
, smod
) where

import Control.Monad
  (guard)
import Control.Monad.State
  (State, get, put, modify, runStateT)
import Control.Monad.Trans.Maybe
  (MaybeT, runMaybeT)
import Data.Functor.Identity
  (runIdentity)
import Data.List
  (uncons)
import Data.Map as M
import Data.Maybe
  (isJust)

type RegisterState b = MaybeT (State (M.Map Int Int)) b

runCmds
  :: RegisterState b
  -> M.Map Int Int
  -> Maybe Int
runCmds cmds map = do
  let result = runIdentity (runStateT (runMaybeT cmds) map)
  let errored = fst result
  let reg0 = (snd result) ! 0
  case errored of
    Nothing -> Nothing
    Just _ -> (Just reg0)

load :: Int -> RegisterState ()
load x = do
  map <- get
  put (insert 0 x map)

write :: Int -> RegisterState ()
write reg = do
  map <- get
  let sreg0 = M.lookup 0 map
  guard $ isJust sreg0
  let reg0 = maybe 0 id sreg0
  put (insert reg reg0 map)
  
read :: Int -> RegisterState ()
read reg = do
  map <- get
  let snewReg0 = M.lookup reg map
  guard $ isJust snewReg0
  let newReg0 = maybe 0 id snewReg0
  put (insert 0 newReg0 map)
  
unop_cond
  :: (Int -> Bool)
  -> (Int -> Int)
  -> RegisterState ()
unop_cond p f = do
  map <- get
  let sreg0 = M.lookup 0 map
  guard $ isJust sreg0
  let reg0 = maybe 0 id sreg0
  guard $ p reg0
  put (insert 0 (f reg0) map)

unop :: (Int -> Int) -> RegisterState ()
unop = unop_cond (const True)

negt = unop (negate)
sign = unop (signum)
inc = unop (1+)
dec = unop ((-1)+)

binop_cond
  :: Int -> (Int -> Int -> Bool)
  -> (Int -> Int -> Int)
  -> RegisterState ()
binop_cond reg p f = do
  map <- get
  let sreg0 = M.lookup 0 map
  guard $ isJust sreg0
  let reg0 = maybe 0 id sreg0
  let sregX = M.lookup reg map
  guard $ isJust sregX
  let regX = maybe 0 id sregX
  guard $ p reg0 regX
  put (insert 0 (f reg0 regX) map)

binop :: Int -> (Int -> Int -> Int) -> RegisterState ()
binop reg = binop_cond reg (const $ const True)

non_zero_2nd = const $ (/=0)

add reg = binop reg (+)
sub reg = binop reg (-)
mul reg = binop reg (*)
smod reg = binop_cond reg non_zero_2nd mod
sdiv reg = binop_cond reg non_zero_2nd div
