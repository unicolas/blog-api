module Main (main) where

import Controllers.Api (api, apiServer)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

main :: IO ()
main = do
  run 8000 (serve api apiServer)
