-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the AutoProg APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Defs
import Parser
import Resolver
import Coder

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testGroup "Parser" [

    testGroup "Name Tests" [
      testCase "Simple var name" $
        parseStringType "a" @?= Right pt2,
      testCase "Var name with all possible chars" $        
        parseStringType "b1_'23" @?= Right tVName0,
      testCase "Var name with illegal char" $
        case parseStringType "b/" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Simple con name" $
        parseStringType "A" @?= Right tCName0,
      testCase "Con name with all possible chars" $        
        parseStringType "B1_'23" @?= Right tCName1,
      testCase "Con name with illegal char" $        
        case parseStringType "B/" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword type" $
        case parseStringType "B type" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword newtype" $
        case parseStringType "B newtype" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword data" $
        case parseStringType "B data" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword type in middle of name" $        
        parseStringType "type24" @?= Right tVName1,
      testCase "Keyword newtype in middle of name" $        
        parseStringType "newtype24" @?= Right tVName2,
      testCase "Keyword data in middle of name" $        
        parseStringType "data24" @?= Right tVName3
    ],

    testGroup "PType tests" [
      testCase "Simple assigning" $
        parseStringType "a->a" @?= Right pt0,
      testCase "Con with var" $
        parseStringType "A a" @?= Right pt1,
      testCase "Simple parentheses" $
        parseStringType "(a)" @?= Right pt2,
      testCase "Simple tuple" $
        parseStringType "(a,b)" @?= Right pt3,
      testCase "Tuple with constructor" $
        parseStringType "(a,B)" @?= Right pt4,
      testCase "Con with multiple types" $
        parseStringType "F x y" @?= Right pt8,
      testCase "Complex 1" $
        parseStringType "F x->(y,A)" @?= Right pt5,
      testCase "Complex 2" $
        parseStringType "F (F x)->(T (y,f5),A b)" @?= Right pt6
    ],

    testGroup "TDeclz tests" [
      testCase "...TDeclz" $
        parseStringTDeclz "type T a = a -> a" @?= Right [td0]
    ],

    testGroup "Whitespace Tests" [
      testCase "Space between assigning" $
        parseStringType "a -> a" @?= Right pt0,
      testCase "Tab between assigning" $   
        parseStringType "a\t" @?= Right pt2,
      testCase "Newline between assigning" $   
        parseStringType "a\n" @?= Right pt2,
      testCase "A lof of whitespace between assigning" $
        parseStringType "\na\n     -> \t\t\n  a  \n" @?= Right pt0,
      testCase "Space at start" $   
        parseStringType " a" @?= Right pt2,
      testCase "Space at end" $   
        parseStringType "a " @?= Right pt2,
      testCase "Space between parantheses" $
        parseStringType " ( a ) " @?= Right pt2,
      testCase "Space between comma" $
        parseStringType "(a , b)" @?= Right pt3,
      testCase "Comment at start" $
        parseStringType "{-Test-}a" @?= Right pt2,
      testCase "Comment at end" $
        parseStringType "a{-Test-}" @?= Right pt2,
      testCase "Comment in middle" $
        parseStringType "a{-Test-}-> a" @?= Right pt0,
      testCase "Comment that does not end" $
        case parseStringType "a{-" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Space between everything pType" $
        parseStringType " F ( F x y ) -> ( T ( y , f5 ) , A b ) " @?= Right pt6
    ],

    testGroup "Disambiguation Tests" [
      testCase "Right-associative arrow" $
        parseStringType "a -> b -> c" @?= Right pt7
    ]
  ],
  testGroup "Resolver" [
    testGroup "resolve" [
      testCase "Test PTVar" $
        resolve tce0 (\x -> return $ STVar (x++"'")) pt2 @?= Right (STVar "a'"),
      testCase "Test STProd" $
        resolve tce0 (\x -> return $ STVar (x++"'")) pt3 @?= Right (STProd (STVar "a'") (STVar "b'")),
      testCase "Test STArrow" $
        resolve tce0 (\x -> return $ STVar (x++"'")) pt0 @?= Right (STArrow (STVar "a'") (STVar "a'")),
      testCase "Test Nested" $
        resolve tce0 (\x -> return $ STVar (x++"'")) pt9 @?= Right (STArrow (STArrow (STVar "a'") (STVar "a'")) (STProd (STArrow (STVar "a'") (STVar "a'")) (STVar "a'"))),
      testCase "Test bad constructor" $
        case resolve tce0 (\x -> return $ STVar (x++"'")) pt1 of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Test bad variable environment" $
        case resolve tce0 (\x -> Left "nope") pt2 of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p
    ],
    testCase "declare" $
      do tce <- declare [td0]
         tf <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T"
         tf [STVar "a'"]
      @?= Right st0
  ],
  testGroup "Coder" [
    testCase "pick" $
      do n <- pick [0,3]
         if n > 0 then return n
         else do m <- pick [4,0]
                 if m > 0 then return m else pick []
      @?= tr0,
    testCase "solutions" $
      solutions tr0 10 Nothing @?= [3,4],
    testCase "produce" $
      do e <- dfs (produce [] st0)
         return $ case e of
                    Lam x (Var x') | x' == x -> e0
                    _ -> e 
      @?= [e0]
    ]]
 where tVName0 = PTVar "b1_'23"
       tVName1 = PTVar "type24"
       tVName2 = PTVar "newtype24"
       tVName3 = PTVar "data24"
       tCName0 = PTApp "A" []
       tCName1 = PTApp "B1_'23" []
       pt0 = PTApp "(->)" [PTVar "a", PTVar "a"]
       pt1 = PTApp "A" [PTVar "a"]
       pt2 = PTVar "a"
       pt3 = PTApp "(,)" [PTVar "a", PTVar "b"]
       pt4 = PTApp "(,)" [PTVar "a", PTApp "B" []]
       pt5 = PTApp "(->)" [PTApp "F" [PTVar "x"], PTApp "(,)" [PTVar "y", PTApp "A" []]]
       pt6 = PTApp "(->)" [PTApp "F" [PTApp "F" [PTVar "x", PTVar "y"]], PTApp "(,)" [PTApp "T" [PTApp "(,)" [PTVar "y", PTVar "f5"]], PTApp "A" [PTVar "b"]]]
       pt7 = PTApp "(->)" [PTVar "a", PTApp "(->)" [PTVar "b", PTVar "c"]]
       pt8 = PTApp "F" [PTVar "x", PTVar "y"]
       pt9 = PTApp "(->)" [PTApp "(->)" [PTVar "a", PTVar "a"], PTApp "(,)" [PTApp "(->)" [PTVar "a", PTVar "a"], PTVar "a"]]
       td0 = TDSyn ("T", ["a"]) pt0
       st0 = STArrow (STVar "a'") (STVar "a'")
       tr0 = Choice [Choice [Found 4, Choice []], Found 3]
       dfs (Found a) = [a]
       dfs (Choice ts) = concatMap dfs ts
       e0 = Lam "X" (Var "X")
