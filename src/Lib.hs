module Lib (main) where
import           Cli                 (Arguments (_commands, _ports, _target, _volumes),
                                      Port (Port), Volume (Volume))
import qualified Cli
import           Command             (Command, run, safeIO)
import           Control.Concurrent  (threadDelay)
import           Control.Monad       (unless)
import           Data.Functor        (void)
import           Data.List           (intersperse)
import           Data.Maybe          (fromMaybe)
import           Docker              (Container, Image, Tag, isRunning, network,
                                      portBinding, volumeBinding)
import qualified Docker
import           Options.Applicative (execParser)

-- Constants

nginxImage :: Image
nginxImage =
    Docker.officialImage "nginx"


nginxTarget :: Docker.Target
nginxTarget =
    Docker.target nginxImage $ Docker.tag "latest"

wait :: String -> IO ()
wait description =
    do
        putStrLn $ "Waiting for " ++ show waitTime ++ " seconds " ++ description
        threadDelay $ waitTime * 1000 * 1000
    where
        waitTime = 3

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
            commands = _commands args

        v <- run $ deploy ports volumes image tag commands

        putStrLn "-------------------"
        case v of
            Right _ ->
                putStrLn "Success"

            Left stdout ->
                putStrLn "Failure" >>
                putStrLn stdout


deploy :: [(Port, Port)] -> [(Volume, Volume)] -> Image -> Tag -> [String] -> Command ()
deploy ports volumes image tag commands =
    do
        mRunningColor <- runningColor image

        let
            newColor = maybe Blue alternate mRunningColor
            target = Docker.target image tag

        net <- network image
        safeIO $ putStrLn $ "Starting " ++ show newColor ++ " image."
        Docker.run (Just net) volumeBinds [] target (toContainer image newColor) commands

        -- This wait allows some time for the server running in the
        -- new image to kick up and be ready to answer to requests
        safeIO $ wait "for server to start"

        {-
            SMOKE TESTS GO HERE
        -}

        safeIO $ putStrLn $ "Switching proxy to " ++ show newColor
        runProxy image ports newColor

        case mRunningColor of
            Just color -> do
                safeIO $ wait $ "for " ++ show color ++ " server to finish handling its requests"
                void $ Docker.kill $ toContainer image color
                safeIO $ putStrLn $ show color ++ " container killed"
                return ()

            Nothing -> return ()

        safeIO $ putStrLn $ show newColor ++ " is now the main container"
        where
            volumeBinds = toVolumeBinding <$> volumes

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
        isContainerRunning <- isRunning proxyContainer
        unless isContainerRunning $
            do
                net <- network image
                void $ Docker.run
                    (Just net)
                    volumeBinds
                    portBinds
                    nginxTarget
                    proxyContainer
                    []

        setProxyConfig proxyContainer image ports color
        void $ Docker.exec proxyContainer ["service", "nginx", "reload"] ""

        where
            proxyContainer = Docker.container image "PROXY"

            volumeBinds = []

            portBinds = toPortBinding <$> ports


{- Will take care of the directory and file -}
setProxyConfig :: Docker.Container -> Docker.Image -> [(Port, Port)] -> Color -> Command ()
setProxyConfig proxyContainer image ports color =
    void $ Docker.exec proxyContainer ["tee",  "/etc/nginx/conf.d/default.conf"] config
    where
        config = mconcat $ intersperse "\n" $ proxyConfig color image <$> ports


proxyConfig :: Color -> Image -> (Port, Port)  -> String
proxyConfig color image (_, Port port) =
    unlines
        [ "server"
        , "{"
        , "    listen " ++ show port ++ ";"
        , "    location /"
        , "    {"
        , "        proxy_pass " ++ toUrl color ++ ";"
        , "    }"
        , "    location /stage"
        , "    {"
        , "        proxy_pass " ++ toUrl (alternate color) ++ ";"
        , "    }"
        , "}"
        ]
    where
        toUrl aColor =
            "http://"++ show (toContainer image aColor) ++ ":" ++ show port ++ "/"


alternate :: Color -> Color
alternate Green = Blue
alternate Blue  = Green
