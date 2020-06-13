import           Lib
import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                , TestTree
                                                )
import           Test.Tasty.HUnit
import           Control.Monad.State

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup
  "Unit Tests"
  [
    testGroup "parse" [
      testCase "Parse command  'd'" $
        fst (parse "d" :: Parse Command) @?= Just (NoAddr, 'd', "")
    , testCase "Parse Address  '2'" $
        fst (parse "2" :: Parse OptAddr2) @?= Just (Addr1 $ LineNum 2)
    , testCase "Parse Address  '2 5'" $
        fst (parse "2 5" :: Parse OptAddr2) @?= Just (Addr2 (LineNum 2) (LineNum 5))
    , testCase "Parse Address  ''" $
        fst (parse "" :: Parse OptAddr2) @?= Just NoAddr
    , testCase "Parse Char 'd'" $
        fst (parse "d" :: Parse Char) @?= Just 'd'
    , testCase "Parse empty script" $
        parseScript "" @?= []
    , testCase "Parse script '2,4 d'" $
        parseScript "2,4 d" @?= [(Addr2 (LineNum 2) (LineNum 4), 'd', "")]
    ]
  , testGroup "checkAddr" [
  ]
  , testGroup "doCycle" [
      testCase "empty script" $
        evalState (doCycle []) (defaultState { patternSpace = "input" }) @?= "input\n"
    , testCase "inside address range" $
        (length . insideRanges) (execState
            (doCycle [(Addr2 (LineNum 1) (LineNum 2), 'd', "")]) $ defaultState { lineNum = 1 })
          @?= 1
  ]
  , testCase "executeSed empty script" $
      evalState (executeSed [] (["input"], "")) defaultState @?= "input\n"
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
    ]
  ]
