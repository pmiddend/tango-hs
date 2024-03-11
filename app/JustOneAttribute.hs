import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Array (withArray)
import Tango (tango_start_server)

main :: IO ()
main = do
  withCString "JustOneAttribute" $ \first -> withCString "testdevice" $ \second -> do
    withArray [first, second] $ \a -> tango_start_server 2 a
