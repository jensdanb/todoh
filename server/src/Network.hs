{-# LANGUAGE OverloadedStrings #-}
module Network where

import Servant (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy, CorsResourcePolicy(..), simpleHeaders, Origin)
import Network.Wai (Middleware)
import Data.ByteString.Char8 (pack)

localHost, localIp, hetznerHsDockerTest, jensDNS :: String
localHost = "localhost"
localIp = "192.168.1.86"
hetznerHsDockerTest = "46.62.152.102"
jensDNS = "jensdanbolt.no"

toOrigin :: String -> Origin
toOrigin = pack . ("http://" <>)

hosts, ports :: [Origin]
hosts = map toOrigin [localHost, localIp, hetznerHsDockerTest, jensDNS]
ports = [":5173", ":5050", ":80", ":443", ""]
allHostPorts = [host<>port | host <- hosts, port <- ports]

runServer :: Middleware -> Application -> Int -> IO ()
runServer mWare app portNr = run portNr (mWare app)

runServerSimpleCors :: Application -> Int -> IO ()
runServerSimpleCors = runServer simpleCors

myCorsPolicy :: Network.Wai.Middleware.Cors.CorsResourcePolicy
myCorsPolicy = simpleCorsResourcePolicy { corsOrigins = Just (allHostPorts, True)
                                        , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
                                        , corsRequestHeaders = ["Authorization", "Content-Type"] }

myCors :: Middleware
myCors = cors $ const (Just myCorsPolicy)

runServerWithCors :: Application -> Int -> IO ()
runServerWithCors = runServer myCors


withApp :: Application -> (Warp.Port -> IO ()) -> IO ()
withApp app = Warp.testWithApplication (pure app)
