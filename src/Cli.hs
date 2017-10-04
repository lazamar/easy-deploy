module Cli where

import Control.Applicative (many, some, (<|>))
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import qualified Docker
import Options.Applicative
    ( Parser
    , ReadM
    , argument
    , auto
    , eitherReader
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , showDefault
    , str
    , switch
    , value
    , (<**>)
    )
import Text.Read (readEither)

data Arguments = Arguments
  { _ports   :: [PortBinding]
  , _volumes :: [VolumeBinding]
  , _target  :: Docker.Target
  }
  deriving (Show)



newtype Port = Port Int
  deriving (Show, Read)

newtype Volume = Volume String
    deriving (Show, Read)

data PortBinding = PortBinding Port Port
    deriving (Show, Read)

data VolumeBinding = VolumeBinding Volume Volume
    deriving (Show, Read)


program =
    info
    (argumentsParser <**> helper)
    $ fullDesc
        -- <> progDesc "Switch a running docker container with a new version without downtime."
        <> header "Switch a running docker container with a new version without downtime."



argumentsParser :: Parser Arguments
argumentsParser =
    Arguments
      <$> some
            (option parsePortBinding
                $ long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Ports to bind. Just like in Docker"
             )
      <*> many
            (option parseVolumeBinding
                $ long "volume"
                <> short 'v'
                <> metavar "VOLUME"
                <> help "Volume to bind. Just like in Docker"
             )
     <*> argument parseDockerTarget
            (   metavar "TARGET"
               <> help "Volume to bind. Just like in Docker"
            )

parsePortBinding :: ReadM PortBinding
parsePortBinding =
    eitherReader $ colonSeparated toPortBinding
    where
        toPortBinding s1 s2= do
            p1 <- readPort s1
            p2 <- readPort s2
            return $ PortBinding p1 p2

parseVolumeBinding :: ReadM VolumeBinding
parseVolumeBinding =
    eitherReader $ colonSeparated toVolumeBinding
    where
        toVolumeBinding s1 s2 =
            return $ VolumeBinding (Volume s1) (Volume s2)


parseDockerTarget :: ReadM Docker.Target
parseDockerTarget =
    eitherReader toTarget
    where
        toTarget s =
            colonSeparated toUserTarget s <|> return (toOfficialTarget s)

        toUserTarget :: String -> String -> Either String Docker.Target
        toUserTarget s1 s2 = do
            img <- separatedBy "/" (\s1 s2 -> return $ Docker.userImage s1 s2) s1
            return $ Docker.target img (Docker.tag s2)

        toOfficialTarget s =
            Docker.target
                (Docker.officialImage s)
                (Docker.tag "latest")


readPort :: String -> Either String Port
readPort v =
    case readEither v of
        Right int ->
            return $ Port int
        Left _ ->
            Left $ "Unable to parse port " ++ v


colonSeparated :: Read a => (String -> String -> Either String a) -> String -> Either String a
colonSeparated = separatedBy ":"


separatedBy :: Read a => String -> (String -> String -> Either String a) -> String -> Either String a
separatedBy separator toType v =
    case splitOn separator v of
        [raw1, raw2] -> toType raw1 raw2
        _            -> Left $ "Unable to parse flag: " ++ v
