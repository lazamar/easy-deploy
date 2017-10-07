module Command
    ( cmd
    , run
    , ignoreResult
    , safeIO
    , Command
    ) where

import           Control.Exception          (catch)
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.Trans.Either as EitherT
import           Data.Either                (either)
import           Data.Functor               (void)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess))
import           System.Process             (readProcessWithExitCode)

type Command a = EitherT.EitherT String IO a

{- Create a command to be executed witn "run" -}
cmd :: String -> [String] -> String -> Command String
cmd program args stdin =
    do
        -- safeIO $ putStrLn $ "Command       : " ++ program ++ mconcat  (fmap (" " ++) args) ++ " " ++ stdin
        (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode program args stdin
        -- safeIO $ putStrLn $ "Command STDERR: " ++ stderr
        -- safeIO $ putStrLn $ "Command STDOUT: " ++ stdout
        -- safeIO $ putStrLn $ "Command EXITCODE:" ++ show exitCode
        case exitCode of
            ExitSuccess ->
                EitherT.right stdout

            ExitFailure code ->
                EitherT.left errorMsg
                where
                    errorMsg =
                        stdout
                        ++ "\n"
                        ++ "exit status: "
                        ++ show code
                        ++ "\n"
                        ++ stderr

{- Make the command happen -}
run :: Command a -> IO (Either String a)
run =
    EitherT.runEitherT

{- Continues a computation even if a cmd fails -}
ignoreResult :: Command a -> Command ()
ignoreResult command =
    EitherT.mapEitherT errorToSuccess command
    where
        errorToSuccess io =
            let
                handleErr :: IOError -> IO (Either String ())
                handleErr _ =
                    return $ return ()
                runIO =
                    void . Right <$> io
            in
                liftIO $ catch runIO handleErr

safeIO :: IO a -> Command a
safeIO io =
    do
        res <- liftIO $ catch runIO handleErr
        either EitherT.left EitherT.right res
        where
            handleErr :: IOError -> IO (Either String a)
            handleErr =
                return . Left . show
            runIO =
                Right <$> io
