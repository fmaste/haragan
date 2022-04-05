{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Criterion.Main (defaultMain, bgroup, bench, whnf, run)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Binary.Get as Get
import Network.HTTP.Request.Examples
import Network.HTTP.Request.Parser.Binary

--------------------------------------------------------------------------------

-- http://hackage.haskell.org/package/criterion
-- http://www.serpentine.com/blog/2009/09/29/criterion-a-new-benchmarking-library-for-haskell/
-- http://lambda.jstolarek.com/2012/10/code-benchmarking-in-haskell/

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [
         bgroup "Request binary parser" 
			[
				bench "Parse Chrome GET" $ 
					run (whnf (Get.runGet getRequest) (fromStrict chrome)) 10000,
				bench "Parse Firefox GET" $ 
					run (whnf (Get.runGet getRequest) (fromStrict firefox)) 10000
			]
       ]
