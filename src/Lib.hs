module Lib (main) where
import qualified Cli
import Command (Command, run, safeIO)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (intersperse)
import Data.List.Split (splitOn)
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

newtype Port = Port Int
    deriving (Show, Read)


data Flag
    = FPort Port Port
    | FVolume String String
        deriving (Show, Read)


cliOptions =
    [ Option "p" (ReqArg (parseFlag readPort FPort) "PORT") "port bindings. Just like in Docker"
    , Option "v" (ReqArg (parseFlag return FVolume) "VOLUME") "volume bindings. Just like in Docker"
    ]


readPort v =
    case readEither v of
        Right int ->
            return $ Port int
        Left _ ->
            Left $ "Unable to parse port " ++ v


parseFlag :: Read a => (String -> Either String a) -> (a -> a -> Flag) -> String -> Either String Flag
parseFlag readRaw toFlag v =
    case splitOn ":" v of
        [raw1, raw2] ->
            do
                p1 <- readRaw raw1
                p2 <- readRaw raw2
                return $ toFlag p1 p2
        _ ->
            Left $ "Unable to parse flag: " ++ v

readArguments :: IO (Either String ([Flag], [String]))
readArguments =
    toEitherArgs <$> getArgs
    where
        toEitherArgs args =
            case getOpt RequireOrder cliOptions args of
                (flags, args, []) ->
                    flip (,) args <$> sequence flags

                (_,_, errs) ->
                    Left $ mconcat errs

main :: IO ()
main =
    do
        args <- execParser Cli.program
        print args

        -- args <- readArguments
        -- print args
        -- v <- run $ deploy
        --     [Port 8080]
        --     (Docker.userImage "lazamar" "lazamar.co.uk")
        --     (Docker.tag "latest")
        -- putStrLn "-------------------"
        -- case v of
        --     Right _ ->
        --         putStrLn "Success"
        --
        --     Left v ->
        --         putStrLn "Failure" >>
        --         putStrLn v


deploy :: [Port] -> Image -> Tag -> Command ()
deploy ports image tag =
    do
        mRunningColor <- runningColor image

        let
            newColor = maybe Blue alternate mRunningColor
            target = Docker.target image tag

        net <- network image
        safeIO $ putStrLn $ "Starting " ++ show newColor ++ " image."
        Docker.run (Just net) volumes ports target (toContainer image newColor)

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
                Docker.kill $ toContainer image color
                safeIO $ putStrLn $ show color ++ " container killed"
                return ()

            Nothing -> return ()

        where
            volumes = [volumeBinding "/home/marcelo/Programs/Projects/lazamar.co.uk" "/home/app"]
            ports = []




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
runProxy :: Image -> [Port] -> Color -> Command ()
runProxy image ports color =
    do
        setProxyConfig image ports color
        isContainerRunning <- isRunning proxyContainer
        if isContainerRunning then
            Docker.exec proxyContainer ["service", "nginx", "reload"]
        else
            do
                net <- network image
                Docker.run  (Just net) volumes portBindings nginxTarget proxyContainer

        where
            proxyContainer =
                Docker.container image "PROXY"

            volumes =
                [ volumeBinding (proxyDir image) nginxConfigFolder ]

            portBindings =
                (\(Port p) -> portBinding p p) <$> ports




{- Will take care of the directory and file -}
setProxyConfig :: Image -> [Port] -> Color -> Command ()
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


proxyConfig :: Color -> Image -> Port  -> String
proxyConfig color image (Port port) =
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
