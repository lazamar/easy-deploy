module Lib (main) where
import Command (Command, run, safeIO)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Docker (isRunning, network, portBinding, volumeBinding)
import qualified Docker
import System.Directory (copyFile, createDirectoryIfMissing)

-- Constants

tempFolder :: String
tempFolder =
    "/home/marcelo/Programs/Projects/easy-deploy/nginx/proxies"

nginxConfigFolder :: String
nginxConfigFolder =
    "/etc/nginx/conf.d/"

nginxImage :: Docker.Image
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

main :: IO ()
main =
    do
        v <- run $ deploy
            (Docker.userImage "lazamar" "lazamar.co.uk")
            (Docker.tag "latest")
        putStrLn "-------------------"
        case v of
            Right v ->
                putStrLn "Success" >>
                putStrLn v
            Left v ->
                putStrLn "Failure" >>
                putStrLn v

{-
    Deployment pseudocode
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
-}
deploy :: Docker.Image -> Docker.Tag -> Command String
deploy image tag =
    do
        let
            target = Docker.target image tag

        -- pull latest version from docker
        safeIO $ putStrLn $ "Pulling image for " ++ show image ++ " from server"
        Docker.pull target
        mRunningColor <- runningColor image
        let
            newColor =
                case mRunningColor of
                    Just Blue -> Green
                    _         -> Blue

        net <- network image
        safeIO $ putStrLn $ "Starting " ++ show newColor ++ " image."
        Docker.run (Just net) volumes ports target (toContainer image newColor)

        -- This wait allows some time for the server running in the
        -- new image to kick up and be ready to answer to requests
        safeIO $ putStrLn "Waiting for 5 seconds before switching"
        safeIO $ threadDelay $ 5 * 1000 * 1000

        safeIO $ putStrLn $ "Switching proxy to " ++ show newColor
        setActiveColor image newColor

        safeIO $ putStrLn "Reloading proxy"
        runProxy image (Port 8080) newColor

        case mRunningColor of
            Just color -> do
                Docker.kill $ toContainer image color
                safeIO $ putStrLn $ show color ++ " container killed"
                return ""

            Nothing -> return ""

        return ""

        where
            volumes = [volumeBinding "/home/marcelo/Programs/Projects/lazamar.co.uk" "/home/app"]
            ports = []




runningColor :: Docker.Image -> Command (Maybe Color)
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



toContainer :: Docker.Image -> Color -> Docker.Container
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
runProxy :: Docker.Image -> Port -> Color -> Command ()
runProxy targetImage (Port targetPort) color =
    do
        setActiveColor targetImage color
        isContainerRunning <- isRunning proxyContainer
        if isContainerRunning then
            Docker.exec proxyContainer ["service", "nginx", "reload"]
        else
            do
                net <- network targetImage
                Docker.run  (Just net) volumes ports nginxTarget proxyContainer

        where
            proxyContainer =
                Docker.container targetImage "PROXY"

            volumes =
                [ volumeBinding (proxyDir targetImage) nginxConfigFolder ]

            ports =
                [ portBinding targetPort 8080 ]


{- Will take care of the directory and file -}
setActiveColor :: Docker.Image -> Color -> Command ()
setActiveColor image color =
    safeIO $
        do
            createDirectoryIfMissing True proxyFolder
            copyFile colorFile destinationPath
        where
            colorFile = colorSourceFile color
            proxyFolder = proxyDir image
            destinationFileName = "default.conf"
            destinationPath = proxyFolder ++ "/" ++ destinationFileName

colorSourceFile :: Color -> String
colorSourceFile color =
    case color of
        Green ->
            "/home/marcelo/Programs/Projects/easy-deploy/nginx/conf.d/green-default.conf"

        Blue ->
            "/home/marcelo/Programs/Projects/easy-deploy/nginx/conf.d/blue-default.conf"

proxyDir :: Docker.Image -> String
proxyDir image =
    tempFolder ++ "/" ++ fmap (\c -> if c == '/' then '-' else c) (show image)
