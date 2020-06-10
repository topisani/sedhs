import Lib
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

main = defaultMain unitTests

foo n = n * 2

unitTests =
  testGroup
    "Unit Tests"
    [
      testCase "nextLine" $
        nextLine "line1" @?= ("line1", ""),
      testCase "Parse command  'd'" $
        fst (parse "d" :: Parse Command) @?= Just (NoAddr, 'd', ""),
      testCase "Parse Address  '2'" $
        fst (parse "2" :: Parse OptAddr2) @?= Just (Addr1 2),
      testCase "Parse Address  '2 5'" $
        fst (parse "2 5" :: Parse OptAddr2) @?= Just (Addr2 2 5),
      testCase "Parse Address  ''" $
        fst (parse "" :: Parse OptAddr2) @?= Just NoAddr,
      testCase "Parse Char 'd'" $
        fst (parse "d" :: Parse Char) @?= Just 'd',
      testCase "Parse empty script" $
        parseScript "" @?= [],
      testCase "doCycle empty script" $
        doCycle [] defaultSed { patternSpace = "input" } @?= "input\n",
      testCase "executeSed empty script" $
        executeSed [] ("input", defaultSed, "") @?= "input\n",
      testCase "execute Empty script" $
        "input" `execute` "" @?= "input\n",
      testCase "Script: d" $
        "input" `execute` "d" @?= "",
      testCase "Commands can be preceded by <blank> or ;" $
        "input" `execute` " ; \td" @?= "",
      testCase "Script: 2d" $
        "line1\nline2\nline3\n" `execute` "2d" @?= "line1\nline3\n"
    ]
