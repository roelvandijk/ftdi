{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , UnicodeSyntax
  #-}

module Main where

-- base
import Data.Bool       ( Bool )
import Data.Char       ( String )
import Data.Function   ( ($) )
import Data.Int        ( Int )
import Data.Ord        ( Ord )
import Data.Word       ( Word8 )
import Prelude         ( Num, Integral, Integer, Bounded
                       , RealFrac, Float, Double
                       , fromRational
                       )
import System.IO       ( IO )
import Text.Show       ( Show )

-- test-framework
import Test.Framework  ( Test, defaultMain, testGroup )

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- QuickCheck
import Test.QuickCheck

-- ftdi
import System.FTDI                  ( ChipType, BaudRate )
import System.FTDI.Properties       ( prop_marshalModemStatus
                                    , prop_unmarshalModemStatus
                                    , prop_baudRateError
                                    )
import System.FTDI.Utils.Properties ( prop_divRndUp_min
                                    , prop_divRndUp_max
                                    , prop_divRndUp_ceilFrac
                                    , prop_divRndUp2
                                    , prop_clamp
                                    )

-- random
import System.Random ( Random )

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
      [ testGroup "error"
        [ test_baudRate_error "Float"  (0.05 ∷ Float)
        , test_baudRate_error "Double" (0.05 ∷ Double)
        ]
      ]
    ]
  , testGroup "utilities"
    [ testGroup "clamp"
      [ unTagged (test_clamp "Int"      ∷ Tagged Int            Test)
      , unTagged (test_clamp "Word8"    ∷ Tagged Word8          Test)
      , unTagged (test_clamp "BaudRate" ∷ Tagged (BaudRate Int) Test)
      ]
    , testGroup "divRndUp"
      [ testGroup "min"
        [ unTagged (test_divRndUp_min "Integer" ∷ Tagged Integer Test)
        , unTagged (test_divRndUp_min "Int"     ∷ Tagged Int     Test)
        , unTagged (test_divRndUp_min "Word8"   ∷ Tagged Word8   Test)
        ]
      , testGroup "max"
        [ unTagged (test_divRndUp_max "Integer" ∷ Tagged Integer Test)
        , unTagged (test_divRndUp_max "Int"     ∷ Tagged Int     Test)
        , unTagged (test_divRndUp_max "Word8"   ∷ Tagged Word8   Test)
        ]
      , testGroup "ceilFrac"
        [ unTagged (test_divRndUp_ceilFrac "Integer" ∷ Tagged Integer Test)
        , unTagged (test_divRndUp_ceilFrac "Int"     ∷ Tagged Int     Test)
        , unTagged (test_divRndUp_ceilFrac "Word8"   ∷ Tagged Word8   Test)
        ]
      , testGroup "alternative2"
        [ unTagged (test_divRndUp_alt2 "Integer" ∷ Tagged Integer Test)
        , unTagged (test_divRndUp_alt2 "Int"     ∷ Tagged Int     Test)
        , unTagged (test_divRndUp_alt2 "Word8"   ∷ Tagged Word8   Test)
        ]
      ]
    ]
  ]

test_baudRate_error ∷ ∀ α. (Arbitrary α, Random α, Num α, RealFrac α) ⇒ String → α → Test
test_baudRate_error n e =
    testProperty n (prop_baudRateError e ∷ ChipType → BaudRate α → Bool)

test_clamp ∷ ∀ α. (Arbitrary α, Bounded α, Ord α, Show α) ⇒ String → Tagged α Test
test_clamp n =
    Tagged $ testProperty n (prop_clamp ∷ α → Property)

test_divRndUp_min ∷ ∀ α. (Arbitrary α, Integral α) ⇒ String → Tagged α Test
test_divRndUp_min n =
    Tagged $ testProperty n (prop_divRndUp_min ∷ α → α → Property)

test_divRndUp_max ∷ ∀ α. (Arbitrary α, Integral α) ⇒ String → Tagged α Test
test_divRndUp_max n =
    Tagged $ testProperty n (prop_divRndUp_max ∷ α → α → Property)

test_divRndUp_ceilFrac ∷ ∀ α. (Arbitrary α, Integral α) ⇒ String → Tagged α Test
test_divRndUp_ceilFrac n =
    Tagged $ testProperty n (prop_divRndUp_ceilFrac ∷ α → α → Property)

test_divRndUp_alt2 ∷ ∀ α. (Arbitrary α, Integral α) ⇒ String → Tagged α Test
test_divRndUp_alt2 n =
    Tagged $ testProperty n (prop_divRndUp2 ∷ α → α → Property)

