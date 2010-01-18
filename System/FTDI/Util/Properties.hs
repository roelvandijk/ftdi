{-# LANGUAGE UnicodeSyntax #-}

module System.FTDI.Util.Properties where

-- base-unicode
import Data.Eq.Unicode  ( (≡), (≢) )
import Data.Ord.Unicode ( (≤), (≥) )
import Prelude.Unicode  ( (⋅), (÷) )

-- ftdi
import System.FTDI.Util ( divRndUp )

-- QuickCheck
import Test.QuickCheck ( Property, (==>) )


-------------------------------------------------------------------------------

prop_divRndUp_min ∷ Integral α ⇒ α → α → Property
prop_divRndUp_min x y = let d = divRndUp x (abs y)
                        in y ≢ 0 ==> d ⋅ abs y ≥ x

prop_divRndUp_max ∷ Integral α ⇒ α → α → Property
prop_divRndUp_max x y = let d = divRndUp x y
                        in y ≢ 0 ==> x `div` y ≤ d

prop_divRndUp_ceilFrac ∷ Integral α ⇒ α → α → Property
prop_divRndUp_ceilFrac x y =
    let x' = fromIntegral x
        y' = fromIntegral y
    in y ≢ 0 ==> divRndUp x y ≡ ceilFrac x' y'

prop_divRndUp2 ∷ Integral α ⇒ α → α → Property
prop_divRndUp2 x y = y ≢ 0 ==> divRndUp x y ≡ divRndUp2 x y

-------------------------------------------------------------------------------

ceilFrac ∷ (Fractional α, RealFrac α, Integral β) ⇒ α → α → β
ceilFrac x y = ceiling $ x ÷ y

divRndUp2 ∷ Integral α ⇒ α → α → α
divRndUp2 x y = let r | mod x y ≡ 0 = 0
                      | otherwise   = 1
                in div x y + r
