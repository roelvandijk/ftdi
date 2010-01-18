{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- test-framework
import Test.Framework ( Test, defaultMain, testGroup )

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- QuickCheck
import Test.QuickCheck

-- ftdi
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

tests =
  [ testGroup "utilities"
    [ testGroup "divRndUp"
      [ testGroup "Integer" $ unTagged (divRndUp ∷ Tagged Integer [Test])
      , testGroup "Int"     $ unTagged (divRndUp ∷ Tagged Int     [Test])
      ]
    ]
  ]

divRndUp ∷ ∀ α. (Arbitrary α, Integral α) ⇒ Tagged α [Test]
divRndUp = Tagged
    [ testProperty "min"          (prop_divRndUp_min      ∷ α → α → Property)
    , testProperty "max"          (prop_divRndUp_max      ∷ α → α → Property)
    , testProperty "ceilFrac"     (prop_divRndUp_ceilFrac ∷ α → α → Property)
    , testProperty "alternative2" (prop_divRndUp2         ∷ α → α → Property)
    ]
