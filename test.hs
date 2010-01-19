{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- base
import Data.Function ( ($) )
import Data.Int      ( Int )
import Data.Word     ( Word8 )
import Prelude       ( Integral, Integer
                     , RealFrac, Float, Double
                     , fromRational
                     )
import System.IO     ( IO )

-- test-framework
import Test.Framework ( Test, defaultMain, testGroup )

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- QuickCheck
import Test.QuickCheck

-- ftdi
import System.FTDI                 ( ChipType )
import System.FTDI.Properties      ( prop_marshalModemStatus
                                   , prop_unmarshalModemStatus
                                   , prop_calcBaudRateDivisor
                                   , prop_baudRateError
                                   )
import System.FTDI.Util.Properties ( prop_divRndUp_min
                                   , prop_divRndUp_max
                                   , prop_divRndUp_ceilFrac
                                   , prop_divRndUp2
                                   )
-- tagged
import Data.Tagged ( Tagged(Tagged, unTagged) )

-------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests =
  [ testGroup "ftdi"
    [ testGroup "modem status"
      [ testProperty "marshal id"   prop_marshalModemStatus
      , testProperty "unmarshal id" prop_unmarshalModemStatus
      ]
    , testGroup "baud rate"
      [testGroup "Float"   $ unTagged (baudRate 0.1 ∷ Tagged Float  [Test])
      , testGroup "Double" $ unTagged (baudRate 0.1 ∷ Tagged Double [Test])
      ]
    ]
  , testGroup "utilities"
    [ testGroup "divRndUp"
      [ testGroup "Integer" $ unTagged (divRndUp ∷ Tagged Integer [Test])
      , testGroup "Int"     $ unTagged (divRndUp ∷ Tagged Int     [Test])
      , testGroup "Word8"   $ unTagged (divRndUp ∷ Tagged Word8   [Test])
      ]
    ]
  ]

baudRate ∷ ∀ α. (Arbitrary α, RealFrac α) ⇒ α → Tagged α [Test]
baudRate e = Tagged
    [ testProperty "divisor" (prop_calcBaudRateDivisor ∷ ChipType → α → Property)
    , testProperty "error"   (prop_baudRateError e ∷ ChipType → α → Property)
    ]

divRndUp ∷ ∀ α. (Arbitrary α, Integral α) ⇒ Tagged α [Test]
divRndUp = Tagged
    [ testProperty "min"          (prop_divRndUp_min      ∷ α → α → Property)
    , testProperty "max"          (prop_divRndUp_max      ∷ α → α → Property)
    , testProperty "ceilFrac"     (prop_divRndUp_ceilFrac ∷ α → α → Property)
    , testProperty "alternative2" (prop_divRndUp2         ∷ α → α → Property)
    ]
