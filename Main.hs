import Control.Applicative ((<$>))
import Prelude hiding (Either(..))
import System.Environment (getArgs)
import System.IO (getContents, stdin)

import Mine
import MCS
import Core
import Flooding
--import Growths

-- I/O Stuff

runTest :: FilePath -> IO ()
runTest f = (readMine <$> readFile f) >>= run >>= putStrLn

main :: IO ()
main = (readMine <$> getContents) >>= run >>= putStrLn
