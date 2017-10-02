module Tester where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Network.HTTP

millisecond = 1000

main =
    loop (50 * millisecond) $
        do
            result <- fetch "http://127.0.0.1:8080"
            print $ take 45 $ drop 116 result

loop int io =
    do
         io
         threadDelay int
         loop int io

fetch url =
    catch performRequest errHandler
    where
        performRequest =
            getResponseBody =<< simpleHTTP (getRequest url)

        errHandler :: IOException -> IO String
        errHandler err =
            return ""
