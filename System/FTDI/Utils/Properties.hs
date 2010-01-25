{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , UnicodeSyntax
  #-}

module System.FTDI.Utils.Properties where

-- base
import Control.Monad ( (>>) )
import Data.Bool     ( otherwise )
import Data.Function ( ($) )
import Data.Ord      ( Ord )
import Prelude       ( Integral, RealFrac, Fractional, Double
                     , Bounded, minBound, maxBound
                     , fromInteger, toInteger, fromIntegral
                     , (+), abs, mod, ceiling, div
                     )

-- base-unicode
import Data.Bool.Unicode ( (∧) )
import Data.Eq.Unicode   ( (≡), (≢) )
import Data.Ord.Unicode  ( (≤), (≥) )
import Prelude.Unicode   ( (⋅), (÷) )

-- ftdi
import System.FTDI.Utils ( clamp, divRndUp )

-- QuickCheck
import Test.QuickCheck ( Property, (==>) )


-------------------------------------------------------------------------------

prop_divRndUp_min ∷ Integral α ⇒ α → α → Property
prop_divRndUp_min x y = y ≢ 0 ==>
    let d  = divRndUp x (abs y)
        d' = toInteger d
        y' = toInteger y
        x' = toInteger x
    in d' ⋅ abs y' ≥ x'

prop_divRndUp_max ∷ Integral α ⇒ α → α → Property
prop_divRndUp_max x y = y ≢ 0 ==>
    let d = divRndUp x y
    in x `div` y ≤ d

prop_divRndUp_ceilFrac ∷ Integral α ⇒ α → α → Property
prop_divRndUp_ceilFrac x y = y ≢ 0 ==>
    let x' = fromIntegral x ∷ Double
        y' = fromIntegral y ∷ Double
    in divRndUp x y ≡ ceilFrac x' y'

prop_divRndUp2 ∷ Integral α ⇒ α → α → Property
prop_divRndUp2 x y = y ≢ 0 ==> divRndUp x y ≡ divRndUp2 x y

prop_clamp ∷ ∀ α. (Bounded α, Ord α) ⇒ α → Property
prop_clamp x = (minBound ∷ α) ≤ (maxBound ∷ α)
               ==> minBound ≤ cx ∧ cx ≤ maxBound
    where cx = clamp x

-------------------------------------------------------------------------------

ceilFrac ∷ (Fractional α, RealFrac α, Integral β) ⇒ α → α → β
ceilFrac x y = ceiling $ x ÷ y

divRndUp2 ∷ Integral α ⇒ α → α → α
divRndUp2 x y = let r | mod x y ≡ 0 = 0
                      | otherwise   = 1
                in div x y + r
