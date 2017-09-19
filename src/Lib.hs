module Lib (main) where
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Either as EitherT
import Data.List.Split (splitOn)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

-- Constants

tempFolder = "/home/marcelo/Programs/Projects/easy-deploy/nginx"

-- Types

newtype DockerTag = DockerTag String

newtype DockerImage = DockerImage String

type Command a = EitherT.EitherT String IO a

-- ================================================

command :: String -> [String] -> String -> Command String
command cmd args stdin =
    do
        (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args stdin
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

run :: Command a -> IO (Either String a)
run =
    EitherT.runEitherT

-- ===================================================

main :: IO ()
main =
    do
        deploy
            (DockerImage "lazamar/lazamar.co.uk")
            (DockerTag "latest")
        v <- run activeNetworks
        print v


deploy :: DockerImage -> DockerTag -> IO ()
deploy (DockerImage _) (DockerTag _) =
    return ()
    -- pull latest version from docker
    -- make sure network is created
    -- if not isRunning nginx then
        -- start nginx service
        -- erase any container with GREEN name
        -- start process on GREEN port
        -- set nginx to GREEN port
    -- else
        -- ACTIVE_NAME = get running process name
        -- NEXT_NAME = calculate next name from process
        -- make sure there are no containers with NEXT_NAME name
        -- erase any container with NEXT_NAME name
        -- start process on NEXT_NAME port
        -- run smoke tests on NEXT_NAME port
        -- set nginx to NEXT_NAME port
        -- stop ACTIVE_NAME process
    -- Erase images older than MAX_IMAGES

activeNetworks :: Command [String]
activeNetworks =
    fmap toList $
    command "awk" ["{ print $2 }"] =<<
    command "docker" ["network", "ls"] ""
     where
            toList =
                filter (not . null) .
                tail .
                splitOn "\n"


createNetwork :: String -> IO ()
createNetwork name =
    return ()
