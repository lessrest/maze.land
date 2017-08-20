{-# Language OverloadedStrings #-}

import qualified Kosmos
import Riga

-- import qualified PGF.Internal as PGF
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Builder as Builder

-- fetch :: String -> (String -> IO ()) -> IO ()
-- fetch = ffi "kosmos_fetch"

main :: IO ()
main = Kosmos.main
  -- fetch "lastadija.pgf" $ \x -> do
  --   print (take 1 x)
