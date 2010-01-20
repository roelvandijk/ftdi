{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module System.FTDI.Properties where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Applicative   ( liftA2 )
import Control.Arrow         ( first )
import Data.Bits             ( (.&.) )
import Data.Bool             ( Bool )
import Data.List             ( map )
import Data.Word             ( Word8 )
import Data.Tuple            ( uncurry )

-- base-unicode
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≤) )
import Prelude.Unicode       ( (÷) )

-- derive
import Data.Derive.Arbitrary ( makeArbitrary )
import Data.DeriveTH         ( derive )

-- ftdi
import System.FTDI           ( ModemStatus(..), ChipType(..)
                             , minBaudRate, maxBaudRate
                             )
import System.FTDI.Internal  ( marshalModemStatus
                             , unmarshalModemStatus
                             , calcBaudRate
                             , calcBaudRateDivisors
                             , supportedSubDivisors
                             )

-- QuickCheck
import Test.QuickCheck       ( Arbitrary, arbitrary, shrink, choose
                             , arbitraryBoundedIntegral
                             , shrinkIntegral, frequency
                             )

-- random
import System.Random         ( Random )


-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

prop_marshalModemStatus ∷ ModemStatus → Bool
prop_marshalModemStatus =
    isIdentity ( uncurry unmarshalModemStatus
               ∘ marshalModemStatus
               )

prop_unmarshalModemStatus ∷ (Word8, Word8) → Bool
prop_unmarshalModemStatus =
    -- The identity only holds when we ignore the 4 least significant bytes.
    isIdentityWith (\x → (ignoreBits x ≡))
                   ( marshalModemStatus
                   ∘ uncurry unmarshalModemStatus
                   ∘ ignoreBits
                   )
    where ignoreBits = first (.&. 0xf0)

prop_calcBaudRateDivisor ∷ RealFrac α ⇒ ChipType → BaudRate α → Bool
prop_calcBaudRateDivisor chip baudRate =
    let subDivs   = supportedSubDivisors chip
        (d, s, e) = calcBaudRateDivisors subDivs baudRate
        baudRate' = calcBaudRate d (fromIntegral s ÷ 8)
    in abs (baudRate' - baudRate) ÷ baudRate ≤ e

prop_baudRateError ∷ RealFrac α ⇒ α → (ChipType → BaudRate α → Bool)
prop_baudRateError maxError = \chip baudRate →
    let subDivs   = supportedSubDivisors chip
        (_, _, e) = calcBaudRateDivisors subDivs baudRate
    in unBaudRate e ≤ maxError


-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

newtype BaudRate α = BaudRate {unBaudRate ∷ α}
    deriving ( Eq, Ord, Num, Enum, Integral
             , Fractional, Real, RealFrac, Show
             , Random
             )

isIdentity ∷ Eq α ⇒ (α → α) → (α → Bool)
isIdentity = isIdentityWith (≡)

isIdentityWith ∷ Eq α ⇒ (α → α → Bool) → (α → α) → (α → Bool)
isIdentityWith eq = liftA2 eq id


-------------------------------------------------------------------------------
-- Arbitrary instances
-------------------------------------------------------------------------------

instance Arbitrary Word8 where
    arbitrary = arbitraryBoundedIntegral
    shrink    = shrinkIntegral

instance Num α ⇒ Bounded (BaudRate α) where
    minBound = BaudRate minBaudRate
    maxBound = BaudRate maxBaudRate

instance (Random α, Num α, Arbitrary α) ⇒ Arbitrary (BaudRate α) where
    arbitrary = frequency [ (1500000 - minBaudRate, choose (minBound, 1500000))
                          , (1, return 2000000)
                          , (1, return 3000000)
                          ]
    shrink    = map BaudRate ∘ shrink ∘ unBaudRate

$( derive makeArbitrary ''ModemStatus )
$( derive makeArbitrary ''ChipType )

