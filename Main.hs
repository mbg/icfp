import Control.Applicative ((<$>))
import Prelude hiding (Either(..))
import System.IO (getContents, stdin)

import Mine
import AStar
import Core
<<<<<<< HEAD
<<<<<<< HEAD
import Flooding
=======

-- I/O Stuff
=======
--import Flooding

-- I/O Stuff

runTest :: String -> IO ()
runTest f = (readMine <$> readFile f) >>= putStrLn . run

main :: IO ()
main = (readMine <$> getContents) >>= putStrLn . run
