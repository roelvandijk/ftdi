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
import Data.Word             ( Word8 )
import Data.Tuple            ( uncurry )

-- base-unicode
import Data.Bool.Unicode     ( (∧) )
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≤), (≥) )
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
import Test.QuickCheck ( Arbitrary, arbitrary, shrink, choose
                       , arbitraryBoundedIntegral, shrinkIntegral
                       , Property, (==>)
                       )


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

prop_calcBaudRateDivisor ∷ RealFrac α ⇒ ChipType → α → Property
prop_calcBaudRateDivisor chip baudRate =
    let subDivs   = supportedSubDivisors chip
        (d, s, e) = calcBaudRateDivisors subDivs baudRate
        baudRate' = calcBaudRate d (fromIntegral s ÷ 8)
    in (baudRate ≥ minBaudRate ∧ baudRate ≤ maxBaudRate)
       ==> abs (baudRate' - baudRate) ÷ baudRate ≤ e

prop_baudRateError ∷ RealFrac α ⇒ α → ChipType → α → Property
prop_baudRateError maxError chip baudRate =
    let subDivs   = supportedSubDivisors chip
        (_, _, e) = calcBaudRateDivisors subDivs baudRate
    in (baudRate ≥ minBaudRate ∧ baudRate ≤ maxBaudRate)
       ==> e ≤ maxError


-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

isIdentity ∷ Eq α ⇒ (α → α) → (α → Bool)
isIdentity = isIdentityWith (≡)

isIdentityWith ∷ Eq α ⇒ (α → α → Bool) → (α → α) → (α → Bool)
isIdentityWith eq = liftA2 eq id


-------------------------------------------------------------------------------
-- Arbitrary instances
-------------------------------------------------------------------------------

$( derive makeArbitrary ''ModemStatus )
$( derive makeArbitrary ''ChipType )

instance Arbitrary Word8 where
    arbitrary = arbitraryBoundedIntegral
    shrink    = shrinkIntegral

