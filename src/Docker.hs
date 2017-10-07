module Docker
    ( tag
    , userImage
    , officialImage
    , container
    , target
    , network
    , portBinding
    , volumeBinding
    , pull
    , run
    , exec
    , isRunning
    , kill
    , killAndRemove
    , Tag
    , Image
    , Target
    , Container
    , PortBinding
    , VolumeBinding
    , Network
    ) where

import           Command          (Command, cmd, ignoreResult, safeIO)
import           Data.Functor     (void)
import           Data.List        (intersperse)
import           Data.Maybe       (maybe)
import           System.Directory (getCurrentDirectory)

newtype Tag = Tag String
    deriving (Read)

data Image
    = OfficialImage String
    | UserImage String String
    deriving (Read)

data Target = Target Image Tag
    deriving (Read)

{- Containers have a qualifier, which is just a string that gets
    added to the container name and allows us to spin up multiple
    containers with the same image, but still being able to
    identify them by name.
-}
data Container = Container Image String

data PortBinding = PortBinding Int Int

data VolumeBinding = VolumeBinding String String

newtype Network = Network Image

-- Constructors

instance Show Target where
    show (Target image tag) =
        show image ++ ":" ++ show tag

instance Show Tag where
    show (Tag name) =
        name

instance Show Image where
    show (OfficialImage name)    = name
    show (UserImage origin name) = origin ++ "/" ++ name

instance Show Container where
    show (Container image qualifier) =
        "container-" ++ qualifier ++ "-" ++ imgName
        where
            imgName =
                case image of
                    OfficialImage name ->
                        name
                    UserImage _ name ->
                        name

instance Show PortBinding where
    show (PortBinding local container) =
        show local ++ ":" ++ show container

instance Show VolumeBinding where
    show (VolumeBinding local container) =
        local ++ ":" ++ container

instance Show Network where
    show (Network image) =
        show image


tag :: String -> Tag
tag = Tag

userImage :: String -> String -> Image
userImage = UserImage

officialImage :: String -> Image
officialImage = OfficialImage

target :: Image -> Tag -> Target
target = Target

container :: Image -> String -> Container
container = Container

portBinding :: Int -> Int -> PortBinding
portBinding = PortBinding

volumeBinding :: String -> String -> VolumeBinding
volumeBinding = VolumeBinding

-- Containers

docker :: [String] -> Command String
docker args = docker_ args ""

docker_ :: [String] -> String -> Command String
docker_ args stdin = cmd "docker" args stdin


kill :: Container -> Command String
kill container =
    docker ["kill", show container]

remove :: Container -> Command String
remove container =
    docker ["rm", show container]

killAndRemove :: Container -> Command ()
killAndRemove container =
    do
        ignoreResult $ docker ["kill", show container ]
        ignoreResult $ docker ["rm", "-f", show container]


{- Will kill and remove any existing container with same name -}
run :: Maybe Network -> [VolumeBinding] -> [PortBinding] -> Target -> Container -> Command ()
run mNetwork volumes ports (Target image tag) container =
    do
        killAndRemove container
        dir <- safeIO getCurrentDirectory
        docker args
        return ()
        where
            toPort (PortBinding p1 p2) =
                show p1 ++ ":" ++ show p2

            toVolume (VolumeBinding v1 v2) =
                v1 ++ ":" ++ v2

            args =
                {- We always make containers detached -}
                mconcat
                    [ [ "run" , "-d" ]
                    , return $ "--name=" ++ show container
                    , (++) "--net=" <$>  maybe [] (return . show) mNetwork
                    , mconcat $ (:) "-v" . return . toVolume <$> volumes
                    , mconcat $ (:) "-p" . return . toPort <$> ports
                    , [ show $ Target image tag ]
                    ]

isRunning :: Container -> Command Bool
isRunning container =
    do
        allRunning <- docker ["ps", "--filter", "name=" ++ show container ]
        return . (<) 1 . length $ filter (== '\n') allRunning

{- Runs instructions in a container that is already running -}
exec :: Container -> [String] -> String -> Command String
exec container commandList stdIn =
    docker_
        [ "exec"
        , "-i"
        , show container
        , "bash"
        , "-c"
        , commands
        ]
        stdIn
        where
            commands = mconcat $ intersperse " " commandList

{-
    It creates a new network if it doesn't exist and
    does nothing if it already exists.
-}
network :: Image -> Command Network
network img =
    do
        ignoreResult $ docker [ "network", "create", show net ]
        return net
    where
        net = Network img

pull :: Target -> Command ()
pull target =
    void $ docker ["pull", show target ]
