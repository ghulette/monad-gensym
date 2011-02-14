import GenSym

br :: String -> String -> String
s1 `br` s2 = s1 ++ "\n" ++ s2

syms :: [String]
syms = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

ex1 :: GenSym String Bool
ex1 = do
  let a = "const jbyte* ${y};" `br`
          "${y} = (*env)->GetStringUTFChars(env, ${x}, NULL);"
  let b = "(*env)->ReleaseStringUTFChars(env, ${x}, ${y});"
  x <- genSym
  y <- genSym
  bindVarsIn [('x',x),('y',y)] $ do
    writeLine "foo(${x},${y})"
  return True

-- ex2 :: CodeGenProc
-- ex2 = CodeGenProc $ \x -> do
--   let a = "jstring ${y};" `br`
--           "${y} = (*env)->NewStringUTF(env, ${x});"
--   clearVars
--   bindVar 'x' x
--   writeStmt a
--   y <- var 'y'
--   return y

main :: IO ()
main = do
  let (_,_,w) = runGenSym syms (ex1 >> ex1)
  putStrLn w
  putStrLn "ok"
