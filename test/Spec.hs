import           Lib
import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                , TestTree
                                                )
import           Test.Tasty.HUnit
import           Control.Monad.State
import           Data.Default

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup
  "Unit Tests"
  [
    testGroup "parse" [
      testCase "Parse command  'd'" $
        evalParser parse "d" @?= Just (('d', NoAddr, NoArg) :: Command)
    , testCase "Parse Address  '2'" $
        evalParser parse "2" @?= Just (Addr1 $ LineNum 2)
    , testCase "Parse Address  '2,5'" $
        evalParser parse "2,5" @?= Just (Addr2 (LineNum 2) (LineNum 5))
    , testCase "Parse Address  ''" $
        evalParser parse "" @?= Just NoAddr
    , testCase "Parse empty script" $
        parseScript "" @?= []
    , testCase "Parse script '2,4 d'" $
        parseScript "2,4 d" @?= [('d', Addr2 (LineNum 2) (LineNum 4), NoArg)]
    , testCase "pOr a" $
        runParser ((eatChar 'a') `pOr` (eatChar 'b')) "a" @?= (Just "a", "")
    , testCase "pOr b" $
        runParser ((eatChar 'a') `pOr` (eatChar 'b')) "b" @?= (Just "b", "")
    , testCase "pOr neither" $
        runParser ((eatChar 'a') `pOr` (eatChar 'b')) "c" @?= (Nothing, "c")
    ]
  , testGroup "checkAddr" [
      testCase "First line of address range" $
        evalState (checkAddr (Addr2 (LineNum 1) (LineNum 2))) (def { lineNum = 1 })
          @?= ACFirst
  ]
  , testGroup "doCycle" [
      testCase "empty script" $
        evalState (doCycle []) (def { patternSpace = "input" }) @?= "input\n"
  ]
  , testCase "executeSed empty script" $
      evalState (executeSed [] (["input"], "")) def @?= "input\n"
  , testGroup "execute" [
      testCase "Empty script" $
        execute "" "input" @?= "input\n"
    , testCase "d function deletes single input" $
        execute "d" "input" @?= ""
    , testCase "Commands can be preceded by <blank> or ;" $
        execute " ; \td" "input" @?= ""
    , testCase "d can be addressed with a single line number" $
        execute "2d" "line1\nline2\nline3\n" @?= "line1\nline3\n"
    , testCase "address and script can be separated with blank chars" $
        execute "2 \t d" "line1\nline2\nline3\n" @?= "line1\nline3\n"
    , testCase "d works on Address range with line numbers" $
        execute "2,4 d" "line1\nline2\nline3\nline4\nline5" @?= "line1\nline5\n"
    , testCase "Second address <= first => only one line selected" $
        execute "2,1 d" "line1\nline2\nline3\nline4" @?= "line1\nline3\nline4\n"
    , testCase "Script: 2p" $
        execute "2p" "line1\nline2\nline3" @?= "line1\nline2\nline2\nline3\n"
    , testCase "c function deletes ps and writes arg to output" $
        execute "c\\\nyes" "line1\nline2\nline3" @?= "yes\nyes\nyes\n"
    , testCase "c with address" $
        execute "2 c\\\nyes" "line1\nline2\nline3" @?= "line1\nyes\nline3\n"
    , testCase "c on range" $
        execute "2,4 c\\\nyes" "line1\nline2\nline3\nline4\nline5\n" @?= "line1\nyes\nline5\n"
    , testCase "c with multiline arg (line preceded by backslash)" $
        execute "2 c\\\nyes\\\nno" "line1\nline2\nline3" @?= "line1\nyes\nno\nline3\n"
    , testCase "c with arg containing escaped backslash" $
        execute "2 c\\\nyes\\\\" "line1\nline2\nline3" @?= "line1\nyes\\\nline3\n"
    , testCase "$ addresses last line" $
        execute "$d" "line1\nline2\n" @?= "line1\n"
    , testCase "$ address works in range" $
        execute "2,$d" "line1\nline2\nline3\nline4" @?= "line1\n"
    , testCase "x: exchange pattern and hold space" $
        execute "x" "line1\nline2\nline3" @?= "\nline1\nline2\n"
    , testCase "Multiple commands separated by newlines" $
        execute "1d\n3d" "line1\nline2\nline3\n" @?= "line2\n"
    , testCase "Multiple commands separated by semicolon" $
        execute "1d;3d" "line1\nline2\nline3\n" @?= "line2\n"
    , testCase "c with semicolon in text" $
        execute "2c\\\nte;st" "line1\nline2\nline3\n" @?= "line1\nte;st\nline3\n"
    , testCase "g: Replace the contents of the pattern space with the hold space" $
        execute "1x;3g" "line1\nline2\nline3" @?= "\nline2\nline1\n"
    , testCase "h: Replace the contents of the hold space with the pattern space" $
        execute "1h;3g" "line1\nline2\nline3" @?= "line1\nline2\nline1\n"
    , testCase "G: Append a <newline> and the hold space to the pattern space" $
        execute "1h;G" "line1\nline2\nline3\n" @?= "line1\nline1\nline2\nline1\nline3\nline1\n"
    , testCase "H: Append a <newline> and the pattern space to the hold space" $
        execute "1h;2H;3x" "line1\nline2\nline3\n" @?= "line1\nline2\nline1\nline2\n"
    , testCase "P: Output the pattern space, up to the first <newline>." $
        execute "1h;2H;3x;3P;3x" "line1\nline2\nline3" @?= "line1\nline2\nline1\nline3\n"
    , testCase "=: Output the line number" $
        execute "=" "one\ntwo\nthree\n" @?= "1\none\n2\ntwo\n3\nthree\n"
    , testCase "# Comments" $
        execute "1d; # This is a ;comment\n 2d" "line1\nline2\nline3" @?= "line3\n"
    , testCase "Script can contain empty lines" $
        execute "1d\n\n\n2d" "line1\nline2\nline3" @?= "line3\n"
    , testGroup "{ command blocks }" [
        testCase "Command block executes all contained commands" $
          execute "{ =; d; }" "line1\nline2" @?= "1\n2\n"
      , testCase "Multiple commands with address in block" $
          execute "{ 3=; 3d; }" "line1\nline2\nline3" @?= "line1\nline2\n3\n"
      , testCase "Command block with address" $
          execute "2 { =; d; }" "line1\nline2" @?= "line1\n2\n"
      , testCase "Command with address in block with address" $
          execute "3 { 3=; 3d; }" "line1\nline2\nline3" @?= "line1\nline2\n3\n"
      , testCase "Command with address in block with address range" $
          execute "2,3 { 3=; 3d; }" "line1\nline2\nline3" @?= "line1\nline2\n3\n"
      , testCase "Nested Command block with conflicting address" $
          execute "2 { 1 { 2d; }; }" "line1\nline2" @?= "line1\nline2\n"
      , testCase "Nested Command block with contained range address" $
          execute "2,$ { 2,3 { d; }; }" "line1\nline2\nline3\nline4" @?= "line1\nline4\n"
      , testCase "Range address contained in block address" $
          execute "2,$ { 2,3d }" "line1\nline2\nline3\nline4" @?= "line1\nline4\n"
      , testCase "Range addresses overlapping block address" $
          execute "2,$ { 1,3d }" "line1\nline2\nline3\nline4" @?= "line1\nline4\n"
      , testCase "Identical nested range address" $
          execute "2,3 { 2,3d }" "line1\nline2\nline3\nline4" @?= "line1\nline4\n"
      ]
    , testCase "i: Insert text" $
        execute "2i\\\nwhoop" "line1\nline2" @?= "line1\nwhoop\nline2\n"
    ]
  ]
