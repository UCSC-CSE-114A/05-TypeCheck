{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf, sort)
import qualified Language.Nano.Types     as Nano
import           Language.Nano.Types ( Type(..) )
import qualified Language.Nano.Eval      as Nano
import qualified Language.Nano.TypeCheck as Nano

main :: IO ()
main = runTests [ unit ]

parse = Nano.parse

-- >>> Nano.lookupId "mickey" []
-- ERROR: unbound variable

unit :: Score -> TestTree
unit sc = testGroup "NANO"
  [ -- lookupID tests
    scoreTest ( uncurry Nano.lookupId
              , ("z1", [("z1", Nano.VInt 0)])
              , Nano.VInt 0
              , 2
              , "lookupId 1")
  , scoreTest ( uncurry Nano.lookupId
            , ("mickey", [])
            , Nano.VErr "unbound variable"
            , 2
            , "lookupId 2")
  , scoreTest ( uncurry Nano.lookupId
            , ("z1", [("x", Nano.VInt 0), ("z1", Nano.VInt 5),
              ("y", Nano.VInt 7), ("z1", Nano.VInt 10)])
            , Nano.VInt 5
            , 2
            , "lookupId 3")
  -- typeCheck tests
  -- 3a tests
  , fileTest  ( "tests/input/3atest1.no"
              , Nano.Ok Nano.TBool
              , 2 )
  , fileTest  ( "tests/input/3atest2.no"
              , Nano.Ok Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/3atest3.no"
              , Nano.Ok Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/3atest4.no"
              , Nano.Ok Nano.TInt
              , 2 )
  -- -- 3b tests
  , fileTest  ( "tests/input/3btest1.no"
              , Nano.Ok Nano.TInt
              , 4 )
  , fileTest  ( "tests/input/3btest2.no"
              , Nano.Ok Nano.TInt
              , 4 )
  -- -- 3c tests
  , fileTest  ( "tests/input/3ctest01.no"
              , Nano.Ok Nano.TBool
              , 2 )
  , fileTest  ( "tests/input/3ctest02.no"
              , Nano.Ok Nano.TBool
              , 2 )
  , fileTest ( "tests/input/3ctest03.no"
              , Nano.Error "type error"
              , 2 )
  , fileTest ( "tests/input/3ctest04.no"
              , Nano.Error "type error"
              , 2 )
  , fileTest  ( "tests/input/3ctest05.no"
              , Nano.Ok Nano.TBool
              , 2 )
  , fileTest ( "tests/input/3ctest06.no"
              , Nano.Error "type error"
              , 2 )
  , fileTest  ( "tests/input/3ctest07.no"
              , Nano.Ok $ Nano.TFun Nano.TInt Nano.TInt
              , 4 )
  , fileTest ( "tests/input/3ctest08.no"
              , Nano.Error "type error"
              , 2 )
  , fileTest  ( "tests/input/3ctest09.no"
              , Nano.Ok $ Nano.TFun Nano.TInt Nano.TInt
              , 4 )
  , fileTest  ( "tests/input/3ctest10.no"
              , Nano.Ok Nano.TInt
              , 4 )
  , fileTest  ( "tests/input/3ctest11.no"
              , Nano.Ok Nano.TInt
              , 2 )
  , fileTest ( "tests/input/3ctest12.no"
              , Nano.Error "type error"
              , 2 )
  -- -- 3d tests
  , fileTest  ( "tests/input/3dtest1.no"
              , Nano.Ok $ Nano.TIntList
              , 4 )
  , fileTest  ( "tests/input/3dtest2.no"
              , Nano.Ok Nano.TInt
              , 4 )
  , fileTest ( "tests/input/3dtest3.no"
              , Nano.Error "type error"
              , 4 )
  , fileTest  ( "tests/input/3dtest4.no"
              , Nano.Error "type error"
              , 6 )
  -- type safe evaluation
  , fileTest'  ( "tests/input/4test1.no"
              , Nano.VInt 7
              , 4 )
  , fileTest'  ( "tests/input/4test2.no"
              , Nano.VInt 10
              , 4 )
  , fileTest'  ( "tests/input/4test3.no"
              , Nano.VInt 25
              , 4 )
  , fileTest'  ( "tests/input/4test4.no"
              , Nano.VInt 3
              , 4 )
  , fileTest'  ( "tests/input/4test5.no"
              , Nano.VPair (Nano.VInt 2) (Nano.VPair (Nano.VInt 3) Nano.VNil)
              , 4 )
  , fileTest'  ( "tests/input/4test6.no"
              , Nano.VInt 120
              , 8 )
  , fileTest'  ( "tests/input/4test7.no"
              , Nano.VPair (Nano.VInt 2) (Nano.VPair (Nano.VInt 3)
                (Nano.VPair (Nano.VInt 4) (Nano.VPair (Nano.VInt 5) Nano.VNil)))
              , 8 )
  , fileTest'  ( "tests/input/4test8.no"
              , Nano.VInt 10
              , 8 )
  , fileTest' ( "tests/input/4test9.no"
              , Nano.VErr "type error"
              , 8
              )
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    failTest :: (Show b, Eq b) => (a -> b, a, String, Int, String) -> TestTree
    failTest (f, x, err, n, msg) = scoreTest' sc (expectError (Nano.Error err) (return . f), x, True, n, msg)

    fileTest  (f, r, n)  = scoreTest' sc (Nano.typeOfFile, f, r, n, "file: " ++ f)
    fileTest' (f, r, n) = scoreTest' sc (Nano.safeExecFile, f, r, n, "file: " ++ f)
    fileTestE (f, e, n) = scoreTest' sc (expectError e Nano.typeOfFile, f, True, n, "file: " ++ f)





expectError :: (Show b) => Nano.TypeOrError -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      -- `catch`
                      -- (return . isInfixOf err . Nano.Error)

env1 :: Nano.Env
env1 =
  [ ("c0", Nano.VInt 0)
  , ("c1", Nano.VInt 1)
  , ("c2", Nano.VInt 2)
  , ("c3", Nano.VInt 3)
  , ("c0", Nano.VInt 4)
  , ("c1", Nano.VInt 5)
  ]

env2 :: Nano.Env
env2 = env1 ++
  [ ("bt", Nano.VBool True)
  , ("bf", Nano.VBool False)
  ]
