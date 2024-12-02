module Main (main) where

import APL.Eval (eval)
import qualified APL.InterpConcurrent as Concurrent
import qualified APL.InterpPure as Pure
import qualified APL.InterpSim as Sim
import APL.Parser (parseAPL)
import System.Environment
  ( getArgs,
    getProgName,
  )
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  args <- getArgs
  let readProg fname = do
        s <- readFile fname
        case parseAPL fname s of
          Left err -> do
            hPutStrLn stderr err
            exitWith $ ExitFailure 1
          Right prog ->
            pure prog
      evalProg fname =
        eval <$> readProg fname
      onRes (Left err) = hPutStrLn stderr err
      onRes (Right v) = hPutStrLn stdout $ show v
  case args of
    ["ast", fname] ->
      print =<< readProg fname
    ["pure", fname] -> do
      m <- evalProg fname
      onRes $ Pure.runEval m
    ["sim", fname] -> do
      m <- evalProg fname
      onRes $ Sim.runEval m
    ["concurrent", fname] -> do
      m <- evalProg fname
      r <- Concurrent.runEval m
      onRes r
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ unwords ["Usage: <ast|pure|sim|concurrent>", prog, " FILE"]
      exitWith $ ExitFailure 1
