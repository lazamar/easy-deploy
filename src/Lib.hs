module Lib (main) where
import Cli (Arguments (_ports, _target, _volumes), Port (Port), Volume (Volume))
import qualified Cli
import Command (Command, run, safeIO)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Docker
    (Container, Image, Tag, isRunning, network, portBinding, volumeBinding)
import qualified Docker
import Options.Applicative (execParser)
import System.Console.GetFlag
    (ArgDescr (ReqArg), ArgOrder (RequireOrder), OptDescr (Option), getOpt)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Environment (getArgs)
import Text.Read (readEither)

-- Constants

tempFolder :: String
tempFolder =
    "/home/marcelo/Programs/Projects/easy-deploy/nginx/proxies"

nginxConfigFolder :: String
nginxConfigFolder =
    "/etc/nginx/conf.d/"

nginxImage :: Image
nginxImage =
    Docker.officialImage "nginx"

nginxTarget :: Docker.Target
nginxTarget =
    Docker.target nginxImage $ Docker.tag "latest"



data Color
    = Green
    | Blue
    deriving (Show)


main :: IO ()
main =
    do
        args <- execParser Cli.program
        let
            ports = _ports args
            volumes = _volumes args
            (image, mTag) = _target args
            tag = fromMaybe (Docker.tag "latest") mTag

        print args

        v <- run $ deploy ports volumes image tag

        putStrLn "-------------------"
        case v of
            Right _ ->
                putStrLn "Success"

            Left v ->
                putStrLn "Failure" >>
                putStrLn v


deploy :: [(Port, Port)] -> [(Volume, Volume)] -> Image -> Tag -> Command ()
deploy ports volumes image tag =
    do
        mRunningColor <- runningColor image

        let
            newColor = maybe Blue alternate mRunningColor
            target = Docker.target image tag

        net <- network image
        safeIO $ putStrLn $ "Starting " ++ show newColor ++ " image."
        Docker.run (Just net) volumeBinds [] target (toContainer image newColor)

        -- This wait allows some time for the server running in the
        -- new image to kick up and be ready to answer to requests
        safeIO $ putStrLn "Waiting for 5 seconds for server to start"
        safeIO $ threadDelay $ 5 * 1000 * 1000

        {-
            SMOKE TESTS GO HERE
        -}

        safeIO $ putStrLn $ "Switching proxy to " ++ show newColor
        runProxy image ports newColor

        case mRunningColor of
            Just color -> do
                safeIO $ putStrLn $ "Waiting for 5 seconds for " ++ show color ++ " server to finish handling its requests"
                Docker.kill $ toContainer image color
                safeIO $ putStrLn $ show color ++ " container killed"
                return ()

            Nothing -> return ()

        where
            volumeBinds = toVolumeBinding <$> volumes

            portBinds = toPortBinding <$> ports

toPortBinding :: (Port, Port) -> Docker.PortBinding
toPortBinding (Port a, Port b) =
    Docker.portBinding a b

toVolumeBinding :: (Volume, Volume) -> Docker.VolumeBinding
toVolumeBinding (Volume a, Volume b) =
    Docker.volumeBinding a b

runningColor :: Image -> Command (Maybe Color)
runningColor img =
    do
        blueRunning <- isRunning $ toContainer img Blue
        greenRunning <- isRunning $ toContainer img Green
        return $
            if blueRunning then
                Just Blue
            else if greenRunning then
                Just Green
            else
                Nothing



toContainer :: Image -> Color -> Container
toContainer img color =
    Docker.container img $ show color

-- ==========================
--      PROXY
-- ==========================
{-
    This will run the proxy if it isn't running and do nothing if it already is.
    Creates all folders and files.
    Will override files if they already exist.

    The proxy will run an nginx image. The image parameter is used
    to determine the proxy name and the network it will run in
-}
runProxy :: Image -> [(Port, Port)] -> Color -> Command ()
runProxy image ports color =
    do
        setProxyConfig image ports color
        isContainerRunning <- isRunning proxyContainer
        if isContainerRunning then
            Docker.exec proxyContainer ["service", "nginx", "reload"]
        else
            do
                net <- network image
                Docker.run
                    (Just net)
                    volumeBinds
                    portBinds
                    nginxTarget
                    proxyContainer

        where
            proxyContainer =
                Docker.container image "PROXY"

            volumeBinds =
                [ volumeBinding (proxyDir image) nginxConfigFolder ]

            portBinds =
                toPortBinding <$> ports




{- Will take care of the directory and file -}
setProxyConfig :: Image -> [(Port, Port)] -> Color -> Command ()
setProxyConfig image ports color =
    safeIO $
        do
            createDirectoryIfMissing True proxyFolder
            writeFile destinationPath config
        where
            config = mconcat $ intersperse "\n" $ proxyConfig color image <$> ports
            proxyFolder = proxyDir image
            destinationPath = proxyFolder ++ "/default.conf"

proxyDir :: Image -> String
proxyDir image =
    tempFolder ++ "/" ++ fmap (\c -> if c == '/' then '-' else c) (show image)


proxyConfig :: Color -> Image -> (Port, Port)  -> String
proxyConfig color image (_, Port port) =
    unlines
        [ "server"
        , "{"
        , "    listen " ++ show port ++ ";"
        , "    location /"
        , "    {"
        , "        proxy_pass " ++ toUrl port image color ++ ";"
        , "    }"
        , "    location /stage"
        , "    {"
        , "        proxy_pass " ++ toUrl port image (alternate color) ++ ";"
        , "    }"
        , "}"
        ]
    where
        toUrl port image color =
            "http://"++ show (toContainer image color) ++ ":" ++ show port ++ "/"


alternate :: Color -> Color
alternate Green = Blue
alternate Blue  = Green
