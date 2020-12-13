import AdvancedRegisterCalc
import Data.Map

cmds = do
  load 2;
  inc;
  write 1;
  write 2;
  add 1;
  mul 2;
  dec

main = do
  putStrLn $ "Result: " ++ (show (runCmds cmds (fromList [(0,0)])))