import Control.Applicative ((<$>))
import Prelude hiding (Either(..))
import System.IO (getContents, stdin)

import Mine
import AStar
import Core
<<<<<<< HEAD
import Flooding
=======

-- loldicks
-- [[KeepCalmCurryOn]]


>>>>>>> dca58b65fe307a07d309ef80478f8de2545d30d6

-- I/O Stuff
main :: IO ()
main = (readMine <$> getContents) >>= putStrLn . run
