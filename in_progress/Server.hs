import Network.Socket
import System.IO
import Parsers
import Control.Applicative

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
           <|> parseAccepted


run :: String -> IO ()
run text = do
    let result = parse parseCommand text
    case (result) of
        []        -> putStrLn "Nothing"
        otherwise -> putStrLn $ (show . fst . head) result






port :: PortNumber
port = 4500

talk :: Handle -> IO ()
talk connectionHandle = do
    hPutStrLn connectionHandle "Hello!"
    response <- hGetLine connectionHandle
    putStrLn $ "Message received: " ++ response
    if response == "quit"
        then do hClose connectionHandle
                return ()
        else do talk connectionHandle


main :: IO ()
main = do
    -- Following this: dev.to/leandronsp/a-crud-journey-in-haskell-part-ii-socket-programming-2po1
    socket <- socket AF_INET Stream 0
    bind socket (SockAddrInet port 0)
    listen socket 2
    putStrLn $ "Listening on port " ++ show port ++ "..."

    -- the server will keep blocked on this line until a new TCP connection is made

    (connection, address) <- accept socket
    putStrLn $ "New connection accepted at " ++ show address
    connectionHandle <- socketToHandle connection ReadWriteMode

    talk connectionHandle
