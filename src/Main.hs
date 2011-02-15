import GenSym

br :: String -> String -> String
s1 `br` s2 = s1 ++ "\n" ++ s2

syms :: [String]
syms = ["_gen" ++ (show i) | i <- [(0 :: Integer)..]]

ex1 :: GenSym [String] String
ex1 = doLocal $ do
  x <- var 'x'
  writeCode "foo(${x},${y},${z})"
  doLocal $ do
    writeCode "bar(${x},${y},${z})"
    z <- var 'z'
    return z

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
  let (_,w) = evalGenSym syms (ex1 >> ex1)
  mapM_ putStrLn w
  putStrLn "ok"
