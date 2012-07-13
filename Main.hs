import Control.Applicative ((<$>))
import Prelude hiding (Either(..))
import System.IO (hGetContents, stdin)
import Control.Monad.State

import Mine
import AStar
import Core

-- loldicks
-- [[KeepCalmCurryOn]]

   

-- I/O Stuff

getStdInContents :: IO [String]
getStdInContents = lines `fmap` hGetContents stdin

strToMine :: [String] -> Mine
strToMine = map (map toObj)

readMap :: IO Mine
readMap = strToMine <$> getStdInContents

main :: IO ()
main = readMap >>= putStrLn . run
