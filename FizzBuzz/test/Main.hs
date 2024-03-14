import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import FizzBuzz (fizzBuzz)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Grupo 1: Numeros primos"
    [ 
       testCase "0" $ assertEqual "FizzBuzz" "cero" (fizzBuzz 0)
    , testCase "3" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 3)
    , testCase "5" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 5)
    , testCase "7" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 7)
    , testCase "11" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 11)
    , testCase "13" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 13)
    , testCase "17" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 17)
    , testCase "19" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 19)
    ]
  , testGroup "Grupo 2: Numeros no primos"
    [ testCase "1" $ assertEqual "uno" "FizzBuzz" (fizzBuzz 1)
    , testCase "4" $ assertEqual "cuatro" "cuatro" (fizzBuzz 4)
    , testCase "6" $ assertEqual "seis" "seis" (fizzBuzz 6)
    , testCase "8" $ assertEqual "ocho" "ocho" (fizzBuzz 8)
    , testCase "9" $ assertEqual "nueve" "nueve" (fizzBuzz 9)
    , testCase "10" $ assertEqual "diez" "diez" (fizzBuzz 10)
    , testCase "12" $ assertEqual "doce" "doce" (fizzBuzz 12)
    , testCase "14" $ assertEqual "catorce" "catorce" (fizzBuzz 14)
    , testCase "15" $ assertEqual "quince" "quince" (fizzBuzz 15)
    , testCase "16" $ assertEqual "dieciseis" "dieciseis" (fizzBuzz 16)
    ]
  , testGroup "Grupo 3: Numeros aleatorios"
    [ testCase "543210" $ assertEqual "quinientos cuarenta y tres mil doscientos diez" "quinientos cuarenta y tres mil doscientos diez" (fizzBuzz 543210)
    , testCase "87629" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 87629)
    , testCase "392154" $ assertEqual "trescientos noventa y dos mil ciento cincuenta y cuatro" "trescientos noventa y dos mil ciento cincuenta y cuatro" (fizzBuzz 392154)
    , testCase "765891" $ assertEqual "setecientos sesenta y cinco mil ochocientos noventa y uno" "setecientos sesenta y cinco mil ochocientos noventa y uno" (fizzBuzz 765891)
    , testCase "123456" $ assertEqual "ciento veintitres mil cuatrocientos cincuenta y seis" "ciento veintitres mil cuatrocientos cincuenta y seis" (fizzBuzz 123456)
    , testCase "987654" $ assertEqual "novecientos ochenta y siete mil seiscientos cincuenta y cuatro" "novecientos ochenta y siete mil seiscientos cincuenta y cuatro" (fizzBuzz 987654)
    , testCase "234567" $ assertEqual "doscientos treinta y cuatro mil quinientos sesenta y siete" "doscientos treinta y cuatro mil quinientos sesenta y siete" (fizzBuzz 234567)
    , testCase "601823" $ assertEqual "FizzBuzz" "FizzBuzz" (fizzBuzz 601823)
    , testCase "345678" $ assertEqual "trescientos cuarenta y cinco mil seiscientos setenta y ocho" "trescientos cuarenta y cinco mil seiscientos setenta y ocho" (fizzBuzz 345678)
    , testCase "876543" $ assertEqual "ochocientos setenta y seis mil quinientos cuarenta y tres" "ochocientos setenta y seis mil quinientos cuarenta y tres" (fizzBuzz 876543)
    ]
    ]