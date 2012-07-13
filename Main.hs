import Control.Applicative ((<$>))
import Prelude hiding (Either(..))
import System.IO (getContents, stdin)

import Mine
import AStar
import Core

-- loldicks
-- [[KeepCalmCurryOn]]



-- I/O Stuff
main :: IO ()
main = (readMine <$> getContents) >>= putStrLn . run