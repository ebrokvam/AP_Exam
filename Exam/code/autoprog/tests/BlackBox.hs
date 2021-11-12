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

tests = testGroup "All tests" [
  testGroup "Parser" [

    testGroup "Name Tests" [
      testCase "Simple var name" $
        parseStringType "a" @?= Right pt2,
      testCase "Var name with all possible chars" $        
        parseStringType "b1_'23" @?= Right (PTVar "b1_'23"),
      testCase "Var name with illegal char" $
        case parseStringType "b/" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Simple con name" $
        parseStringType "A" @?= Right (PTApp "A" []),
      testCase "Con name with all possible chars" $        
        parseStringType "B1_'23" @?= Right (PTApp "B1_'23" []),
      testCase "Con name with illegal char" $        
        case parseStringType "B/" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword type" $
        case parseStringType "type" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword newtype" $
        case parseStringType "newtype" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword data" $
        case parseStringType "data" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Keyword type in middle of name" $        
        parseStringType "type24" @?= Right (PTVar "type24"),
      testCase "Keyword newtype in middle of name" $        
        parseStringType "newtype24" @?= Right (PTVar "newtype24"),
      testCase "Keyword data in middle of name" $        
        parseStringType "data24" @?= Right (PTVar "data24"),
      testCase "pType with all illegal names" $
        case parseStringType "F* x* -> (y*, A*)" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "tDeclz with all illegal names" $
        case parseStringTDeclz "data T* = C* {f*, f* :: a* -> a*}" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p
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
        parseStringType "(a,B)" @?= Right (PTApp "(,)" [PTVar "a", PTApp "B" []]),
      testCase "Con with multiple types" $
        parseStringType "F (X y)" @?= Right (PTApp "F" [PTApp "X" [PTVar "y"]]),
      testCase "Complex 1" $
        parseStringType "F x->(y,A)" @?= Right pt4,
      testCase "Complex 2" $
        parseStringType "F (F x y)->(T (y,f5),A b)" @?= Right pt5
    ],

    testGroup "TDeclz tests" [
      testCase "Empty list of declarations" $
        parseStringTDeclz "" @?= Right [],
      testCase "Parse declare type" $
        parseStringTDeclz "type X a=a->a" @?= Right [td0],
      testCase "Parse declare newtype" $
        parseStringTDeclz "newtype T=B{c ::a}" @?= Right [TDRcd ("T", []) "B" [("c", PTVar "a")]],
      testCase "Parse declare data no fields" $
        parseStringTDeclz "data T=C {}" @?= Right [TDRcd ("T", []) "C" []],
      testCase "Parse declare data one field" $
        parseStringTDeclz "data T=C {f1::a->a}" @?= Right [TDRcd ("T", []) "C" [("f1", pt0)]],
      testCase "Parse data multiple fields" $
        parseStringTDeclz "data T=C {f1, f2::a->a}" @?= Right [TDRcd ("T", []) "C" [("f1", pt0), ("f2", pt0)]],
      testCase "Parse data duplicate name fields" $
        parseStringTDeclz "data T=C {f, f::a->a}" @?= Right [TDRcd ("T", []) "C" [("f", pt0), ("f", pt0)]],
      testCase "Prase synonym types" $
        parseStringTDeclz "type T a b c = a" @?= Right [TDSyn ("T", ["a", "b", "c"]) (PTVar "a")],
      testCase "Prase two declarations" $
        parseStringTDeclz "type T=a; type T=b;" @?= Right [TDSyn ("T", []) (PTVar "a"), TDSyn ("T", []) (PTVar "b")],
      testCase "Ignore semicolon before declaration" $
        parseStringTDeclz ";type X a=a->a" @?= Right [td0],
      testCase "Parse declare with bigger pType" $
        parseStringTDeclz "type T a = F x->(y,A)" @?= Right [TDSyn ("T", ["a"]) pt4]
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
        case parseStringType "a {-" of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "Comment in middle of declaration" $
        parseStringTDeclz "data T=C {{-TODO: add fields-}}" @?= Right [TDRcd ("T", []) "C" []],
      testCase "Space between everything pType" $
        parseStringType " F ( F x y ) -> ( T ( y , f5 ) , A b ) " @?= Right pt5,
      testCase "Space between everything declare type" $
        parseStringTDeclz "   type T  a = a -> a " @?= Right [td0],
      testCase "Space between everything declare newtype" $
        parseStringTDeclz " newtype   T = B { c :: a } " @?= Right [TDRcd ("T", []) "B" [("c", PTVar "a")]],
      testCase "Space between everything declare data" $
        parseStringTDeclz "  \t data T a t1 d = C \n {  f1 ,  f2 :: a -> a }   " @?= Right [TDRcd ("T", ["a", "t1", "d"]) "C" [("f1", pt0), ("f2", pt0)]]
    ],

    testGroup "Disambiguation Tests" [
      testCase "Constructor parentheses" $
        parseStringType "F x y" @?= Right (PTApp "F" [PTVar "x", PTVar "y"]),
      testCase "Constructor tighter than infix" $
        parseStringType "F x y -> z" @?= Right (PTApp "(->)" [PTApp "F" [PTVar "x", PTVar "y"], PTVar "z"]),
      testCase "Right-associative arrow" $
        parseStringType "a -> b -> c" @?= Right (PTApp "(->)" [PTVar "a", PTApp "(->)" [PTVar "b", PTVar "c"]])
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
        resolve tce0 (\x -> return $ STVar (x++"'")) pt6 @?= Right (STArrow (STArrow (STVar "a'") (STVar "a'")) (STProd (STArrow (STVar "a'") (STVar "a'")) (STVar "a'"))),
      testCase "Test non-existent constructor" $
        case resolve tce0 (\x -> return $ STVar (x++"'")) pt1 of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
      testCase "Test bad args in constructor" $
        case resolve tce0 (\x -> return $ STVar (x++"'")) (PTApp "(,)" [PTVar "x"]) of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
      testCase "Test bad variable environment" $
        case resolve tce0 (\x -> Left "nope") pt2 of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
      testCase "Example from spec" $
        resolve tce1 (\x -> return $ STVar x) (PTApp "T" [PTApp "(,)" [PTVar "b", PTVar "b"]]) @?= Right st1
    ],
    testGroup "Declare" [
      testCase "Empty declaration list" $
        case testDeclare [] "X" [STVar "a"] of
          Left _ -> return ()
          Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
      testCase "Declare synonym with var" $
        testDeclare [td2] "T" [STVar "a'"] @?= Right (STVar "a'"),
      testCase "Declare synonym for (->)" $
        testDeclare [td0] "X" [STVar "a'"] @?= Right st0,
      testCase "Declare synonym for (,)" $
        testDeclare [td3] "Y" [STVar "a'"] @?= Right (STProd (STVar "a'") (STVar "a'")),
      testCase "Declare constructor with two type variables" $
        testDeclare [td16] "T" [STVar "a'"] @?= Right (STVar "a'"),
      testCase "Example from specs" $
        testDeclare [td1] "Z" [STProd (STVar "b") (STVar "b")] @?= Right st1,
      testCase "Four declarations" $
        testDeclare [td0, td1, td2, td3] "Z" [STProd (STVar "b") (STVar "b")] @?= Right st1,
      testGroup "Semantics" [
        testCase "Non-distinct type constructor (fail)" $
          case testDeclare [td0, td0] "X" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "Refer to declaration ahead in list (fail)" $
          case testDeclare [td14, td15] "T" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "Refer to previous declaration (success)" $
          testDeclare [td15, td4] "T" [STVar "a"] @?= Right (STProd (STVar "a") (STVar "x")),
        testCase "Non-distinct LHS type-variables (fail)" $
          case testDeclare [td4] "X" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "RHS type-variable not on LHS (fail)" $
          case testDeclare [td5] "X" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "Non-distinct field names (fail)" $
          case testDeclare [td6] "X" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "Same field name in two declarations (fail)" $
          case testDeclare [td7, td8] "A" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "Field name keyword fst (fail)" $
          case testDeclare [td9] "X" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,
        testCase "Field name keyword snd (fail)" $
          case testDeclare [td10] "X" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p,     
        testCase "Matching type and record constructors (succeed)" $
          testDeclare [td11] "T" [STVar "a"] @?= Right (STRcd "T" [("a", STVar "a")]),
        testCase "Matching variable and field names (succeed)" $
          testDeclare [td12] "T" [STVar "a"] @?= Right (STRcd "T" [("t", STVar "a")]),
        testCase "Recursive declaration (fail)" $
          case testDeclare [td13] "T" [STVar "a"] of
            Left _ -> return ()
            Right p -> assertFailure $ "Unexpected resolve: " ++ show p
      ]
    ],
    testCase "Declare, then Resolve example from specs" $
      do tce <- declare [td1]
         st <- resolve tce (\x -> return $ STVar x) (PTApp "T" [PTApp "(,)" [PTVar "b", PTVar "b"]])
         return st
      @?= Right st1
  ],
  testGroup "Coder" [
    testGroup "Pick" [
      testCase "Pick empty" $
        pick ([] :: [Char]) @?= Choice [],
      testCase "Pick one choice" $
        pick [3] @?= Found 3,
      testCase "Pick multiple choices" $
        pick ['a', 'b', 'c', 'd'] @?= Choice [Found 'a', Found 'b', Found 'c', Found 'd'],
      testCase "Pick multiple levels" $
          do n <- pick [0,3]
             if n > 0 then return n
             else pick []
          @?= Choice [Choice [], Found 3],
      testCase "Pick bigger tree" $
          do n <- pick [0,3]
             if n > 0 then return n
             else do m <- pick [4,0]
                     if m > 0 then return m else pick []
          @?= tr0,
      testCase "Pick huge tree" $
          do n <- pick [0,3]
             if n > 0 then return n
             else do m <- pick [4,6]
                     if m > 4 then return m 
                     else do o <- pick [0,3,5,2]
                             if o > 0 then return o
                             else do p <- pick [7,0]
                                     if p > 0 then return p 
                                     else do q <- pick [7,0]
                                             if q > 0 then return q 
                                             else pick [8, 9]
          @?= Choice [Choice [Choice [Choice [Found 7,Choice [Found 7,Choice [Found 8,Found 9]]],Found 3,Found 5,Found 2],Found 6],Found 3]
    ],
    testGroup "solutions" [
      testCase "Simple found 3" $
        solutions (Found "a") 10 Nothing @?= ["a"],
      testCase "Simple empty" $
        solutions (Choice [] :: Tree Int) 10 Nothing @?= [],
      testCase "Nested choices (BFS check)" $
        solutions tr0 10 Nothing @?= [3,4],
      testCase "Nested choices, empty list" $
        solutions (Choice [Choice [], Choice [Choice [], Choice []], Choice [Choice []]] :: Tree Int) 10 Nothing @?= [],
      testCase "Nested 1-5 #1" $
        solutions tr1 10 Nothing @?= [1,2,3,4,5],
      testCase "Nested 1-5 #2" $
        solutions tr2 10 Nothing @?= [1,2,3,4,5],
      testCase "Exceeding n with d is nothing" $
        solutions tr0 1 Nothing @?= [3],
      testCase "Exceeding n with d is 5" $
        solutions tr0 1 (Just 5) @?= [3,5],
      testCase "Exceeding n with bigger tree #1" $
        solutions tr1 4 (Just 6) @?= [1,2,3,4,6],
      testCase "Exceeding n with bigger tree #2" $
        solutions tr2 3 Nothing @?= [1,2,3],
      testCase "n is exact size of list, no d added #1" $
        solutions tr1 5 (Just 6) @?= [1,2,3,4,5],
      testCase "n is exact size of list, no d added #2" $
        solutions tr2 5 (Just 6) @?= [1,2,3,4,5],
      testCase "n solutions to infinite tree" $
        solutions tr3 5 (Just 0) @?= [1,1,1,1,1,0]
    ],
    testGroup "Produce" [
      testCase "produce" $
        do e <- dfs (produce [] st0)
           return $ case e of
                      Lam x (Var x') | x' == x -> e0
                      _ -> e 
        @?= [e0]
    ]
    -- TODO: test combination of modules!!
    ]]
 where pt0 = PTApp "(->)" [PTVar "a", PTVar "a"]
       pt1 = PTApp "A" [PTVar "a"]
       pt2 = PTVar "a"
       pt3 = PTApp "(,)" [PTVar "a", PTVar "b"]
       pt4 = PTApp "(->)" [PTApp "F" [PTVar "x"], PTApp "(,)" [PTVar "y", PTApp "A" []]]
       pt5 = PTApp "(->)" [PTApp "F" [PTApp "F" [PTVar "x", PTVar "y"]], PTApp "(,)" [PTApp "T" [PTApp "(,)" [PTVar "y", PTVar "f5"]], PTApp "A" [PTVar "b"]]]
       pt6 = PTApp "(->)" [PTApp "(->)" [PTVar "a", PTVar "a"], PTApp "(,)" [PTApp "(->)" [PTVar "a", PTVar "a"], PTVar "a"]]
       td0 = TDSyn ("X", ["a"]) pt0
       td1 = TDRcd ("Z", ["a"]) "C" [("x", PTVar "a"), ("f", PTApp "(->)" [PTVar "a", PTVar "a"])]
       td2 = TDSyn ("T", ["a"]) pt2
       td3 = TDSyn ("Y", ["a"]) (PTApp "(,)" [PTVar "a", PTVar "a"])
       td4 = TDSyn ("X", ["a, a"]) (PTApp "(,)" [PTVar "a", PTVar "a"])
       td5 = TDSyn ("X", ["a"]) (PTApp "(,)" [PTVar "b", PTVar "b"])
       td6 = TDRcd ("X", ["a"]) "C" [("x", PTVar "a"), ("x", PTVar "a")]
       td7 = TDRcd ("A", ["a"]) "C" [("x", PTVar "a")]
       td8 = TDRcd ("B", ["a"]) "C" [("x", PTVar "a")]
       td9 = TDRcd ("X", ["a"]) "C" [("fst", PTVar "a")]
       td10 = TDRcd ("X", ["a"]) "C" [("snd", PTVar "a")]
       td11 = TDRcd ("T", ["a"]) "T" [("t", PTVar "a")]
       td12 = TDRcd ("T", ["t"]) "T" [("t", PTVar "t")]
       td13 = TDSyn ("T", ["a"]) (PTApp "(->)" [PTVar "a", PTApp "T" [PTVar "a"]])
       td14 = TDSyn ("T", ["a"]) (PTApp "(->)" [PTVar "a", PTApp "U" [PTVar "a"]])
       td15 = TDSyn ("U", ["x"]) (PTVar "x")
       td16 = TDSyn ("T", ["a", "b"]) (PTApp "(->)" [PTVar "a", PTVar "b"])
       st0 = STArrow (STVar "a'") (STVar "a'")
       st1 = STRcd "C" [("x", STProd (STVar "b") (STVar "b")), ("f", STArrow (STProd (STVar "b") (STVar "b")) (STProd (STVar "b") (STVar "b")))]
       tr0 = Choice [Choice [Found 4, Choice []], Found 3]
       tr1 = Choice [Found 1, Choice [Found 3, Choice [Found 5], Found 4], Found 2]
       tr2 = Choice [Choice [Choice [Choice [Choice [Found 5], Found 4], Found 3], Found 2], Found 1]
       tr3 = Choice [tr3, Found 1]
       dfs (Found a) = [a]
       dfs (Choice ts) = concatMap dfs ts
       e0 = Lam "X" (Var "X")
       
tce1 = tce0 ++ [("T", \ts -> 
  case ts of
    [t] -> return $ STRcd "C" [("x", t), ("f", STArrow t t)]
    _ -> Left "bad args for T")]


testDeclare :: [TDecl] -> TCName -> [SType] -> EM SType
testDeclare ds tc st = do 
  tce <- declare ds
  tf <- case lookup tc tce of Just tf -> return tf; _ -> Left "no T"
  tf st
