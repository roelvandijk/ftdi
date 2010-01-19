{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.FTDI.Util where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Monad             ( (>>) )
import Data.Bool                 ( Bool, otherwise )
import Data.Bits                 ( Bits, (.|.), (.&.)
                                 , complement, setBit
                                 , shiftL, testBit
                                 )
import Data.Eq                   ( (==) )
import Data.Function             ( ($) )
import Data.List                 ( foldr, head, minimumBy, partition, zip )
import Data.Maybe                ( Maybe(Just, Nothing), maybe )
import Data.Ord                  ( Ord, (<), (>), min, max )
import Data.Word                 ( Word8, Word16 )
import Prelude                   ( Enum
                                 , Num, (+), (-), signum, Integral, quotRem, (^)
                                 , RealFrac, Fractional
                                 , fromEnum, fromInteger, fromIntegral
                                 , abs, realToFrac, floor, ceiling
                                 , div, mod, divMod, error
                                 )

-- base-unicode-symbols
import Data.Bool.Unicode         ( (∧) )
import Data.Eq.Unicode           ( (≡), (≢) )
import Data.Function.Unicode     ( (∘) )
import Data.Monoid.Unicode       ( (⊕) )
import Data.Ord.Unicode          ( (≤), (≥) )
import Prelude.Unicode           ( (⋅), (÷) )

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


genFromEnum ∷ (Enum e, Num n) ⇒ e → n
genFromEnum = fromIntegral ∘ fromEnum

orBits ∷ Bits α ⇒ [α] → α
orBits = foldr (.|.) 0

andBits ∷ Bits α ⇒ [α] → α
andBits = foldr (.&.) $ complement 0

prop_clamp ∷ Ord α ⇒ α → α → α → Bool
prop_clamp lo hi x = lo ≤ x ∧ x ≤ hi

clamp ∷ Ord α ⇒ α → α → α → α
clamp lo hi = atLeast lo ∘ atMost hi

atLeast ∷ Ord α ⇒ α → α → α
atLeast = max

atMost ∷ Ord α ⇒ α → α → α
atMost = min

divRndUp  ∷ Integral α ⇒ α → α → α
divRndUp x y = let (d, m) = x `divMod` y
               in d + if m ≢ 0 then 1 else 0
