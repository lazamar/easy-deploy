module Cli where

import           Control.Applicative (many, some, (<|>))
import           Data.List.Split     (splitOn)
import           Data.Semigroup      ((<>))
import qualified Docker
import           Options.Applicative (Parser, ParserInfo, ReadM, argument,
                                      eitherReader, fullDesc, header, help,
                                      helper, info, long, metavar, option,
                                      short, str, (<**>))
import           Text.Read           (readEither)

data Arguments = Arguments
  { _ports    :: [(Port, Port)]
  , _volumes  :: [(Volume, Volume)]
  , _target   :: (Docker.Image, Maybe Docker.Tag)
  , _commands :: [String]
  }
  deriving (Show)



newtype Port = Port Int
  deriving (Show, Read)

newtype Volume = Volume String
    deriving (Show, Read)

program :: ParserInfo Arguments
program =
    info
    (argumentsParser <**> helper)
    $ fullDesc
        -- <> progDesc "Switch a running docker container with a new version without downtime."
        <> header "Switch a running docker container with a new version without downtime."



argumentsParser :: Parser Arguments
argumentsParser =
    Arguments
        <$> (some
                $ option parsePortBinding
                $ long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Ports to bind. Just like in Docker"
            )
        <*> (many
                $ option parseVolumeBinding
                $ long "volume"
                <> short 'v'
                <> metavar "VOLUME"
                <> help "Volume to bind. Just like in Docker"
            )
        <*> (argument parseDockerTarget
                $ metavar "TARGET"
                <> help "Volume to bind. Just like in Docker"
            )
        <*> ( many
                $ argument str
                $ metavar "COMMAND"
                <> help "Commands to send to the docker image"
            )

parsePortBinding :: ReadM (Port, Port)
parsePortBinding =
    eitherReader $ colonSeparated toPortBinding
    where
        toPortBinding s1 s2= do
            p1 <- readPort s1
            p2 <- readPort s2
            return (p1, p2)

parseVolumeBinding :: ReadM (Volume, Volume)
parseVolumeBinding =
    eitherReader $ colonSeparated toVolumeBinding
    where
        toVolumeBinding s1 s2 =
            return (Volume s1, Volume s2)


parseDockerTarget :: ReadM (Docker.Image, Maybe Docker.Tag)
parseDockerTarget =
    eitherReader toTarget
    where
        toTarget s =
            colonSeparated withTag s <|> withoutTag s

        withTag s tagName = do
            img <- toImage s
            return (img, Just $ Docker.tag tagName)

        withoutTag s = do
            img <- toImage s
            return (img, Nothing)

        toImage s =
            separatedBy "/" withUser s <|> withoutUser s

        withUser userName imageName =
            return $ Docker.userImage userName imageName

        withoutUser imageName =
            return $ Docker.officialImage imageName



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
