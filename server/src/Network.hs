{-# LANGUAGE OverloadedStrings #-}
module Network where

import Servant (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy, CorsResourcePolicy(..), simpleHeaders, Origin)
import Network.Wai (Middleware)

localAddr, hetznerHsDockerTest :: Origin
localAddr = "http://localhost:"
hetznerHsDockerTest = "http://46.62.152.102:"

hosts, ports :: [Origin]
hosts = [localAddr, hetznerHsDockerTest]
ports = ["5173", "5050"]
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
