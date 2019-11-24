import Network.Socket
import System.IO
import qualified Control.Exception as E
import Options.Applicative
import Data.Semigroup((<>))
import System.Exit

import MainBrick

type Hostname = String
type Port = String

defaultHostname :: String
defaultHostname = "127.0.0.1"

defaultPortN :: String
defaultPortN = "10042"

srvAddr :: Hostname -> Port -> IO AddrInfo
srvAddr ip port = E.catch 
            (head <$> getAddrInfo Nothing (Just ip) (Just port))
            defaultSrv
    
    where
        defaultSrv :: E.IOException -> IO AddrInfo
        defaultSrv _ = do 
                       defaultAddr <- head <$> getAddrInfo Nothing (Just defaultHostname) (Just defaultPortN)
                       putStrLn "Invalid hostname or port number."
                       putStrLn $ "Using default hostname: " ++ defaultHostname ++ ", " ++ "port: " ++ defaultPortN
                       return defaultAddr

data ServerConf = ServerConf
  { hostname :: String
  , port     :: String
  }

parseCmdArgs :: Parser ServerConf
parseCmdArgs = ServerConf
      <$> strOption
          ( long "address"
         <> metavar "HOSTNAME"
         <> short 'a'
         <> value defaultHostname
         <> showDefault
         <> help "The hostname of golserver" )
      <*> strOption
          ( long "port"
         <> short 'p'
         <> metavar "PORT"
         <> value defaultPortN
         <> showDefault
         <> help "Port number of golserver" )

main :: IO ()
main = mainCmdArgsParsed =<< execParser opts
  where
    opts = info (parseCmdArgs <**> helper)
      ( fullDesc
     <> progDesc "Connect to golserver with supplied arguments"
     <> header "Client application connecting to golserver" )

mainCmdArgsParsed :: ServerConf -> IO ()
mainCmdArgsParsed (ServerConf srv port) = do
                                          addr <- srvAddr srv port
                                          E.bracket (createSck addr) close mainBrick
  where
    createSck :: AddrInfo -> IO Socket
    createSck addr = do
                     sck <- socket (addrFamily addr) Stream defaultProtocol
                     setSocketOption sck ReuseAddr 1
                     E.catch
                        (connect sck (addrAddress addr))
                        (couldNotConnect addr)
                     return sck

    couldNotConnect :: AddrInfo -> E.IOException -> IO ()
    couldNotConnect addr _ = die $ show (addrAddress addr) ++ " does not appear to be valid gol server."
