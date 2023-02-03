module Server where

import Network.Socket
import System.IO
import Control.Concurrent



main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock                              -- unimplemented

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    forkIO (runConn conn)   -- split off each connection into its own thread
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello!"
    hClose hdl



-- Control.Concurrent.Chan takes care of all the messaging between threads for us












data Player = W | B
    deriving (Show, Eq)

type Coordinate = (Int, Int)

data Command =
    Start
  | Accepted
  | Move Coordinate Coordinate
  | Moved Player Coordinate Coordinate
    deriving (Show, Eq)


-- protocol

-- server is running
-- 1 connects to server
-- 1 msg to server: start
-- server msg to 1: player b
-- randomly assigned
-- 2 connects to server
-- 2 msg to server: start
-- server msg to 2: player w

-- b player goes first
-- 1 msg to server: move 0 2 1 3
-- server msg to 1: accepted move
-- server msg to 2: moved b 0 2 1 3
-- 2 msg to server: accepted moved

{- 

parseCoordinate :: Parser Coordinate
parseCoordinate = do
    x <- nat
    space
    y <- nat
    return (x, y)


parseMove :: Parser Command
parseMove = do
    string "move"
    space
    c1 <- parseCoordinate
    space
    c2 <- parseCoordinate
    return (Move c1 c2)


parseStart :: Parser Command
parseStart = do
    string "start"
    return Start

parseAccepted :: Parser Command
parseAccepted = do
    string "accepted"
    return Accepted


parseCommand :: Parser Command
parseCommand = parseMove
           <|> parseStart
           <|> parseAccepted -}


{- run :: String -> IO ()
run text = do
    let result = parse parseCommand text
    case (result) of
        []        -> putStrLn "Nothing"
        otherwise -> putStrLn $ (show . fst . head) result
 -}


