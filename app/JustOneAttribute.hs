import Foreign.C (CLong)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Array (withArray)
import Tango
  ( createGetterWrapper,
    createSetterWrapper,
    tango_init_server,
    tango_set_attribute_getter,
    tango_set_attribute_setter,
    tango_start_server,
  )

attributeGetter :: IO CLong
attributeGetter = do
  putStrLn "reading"
  pure 1339

attributeSetter :: CLong -> IO ()
attributeSetter newValue = do
  putStrLn $ "new value " <> show newValue

main :: IO ()
main = do
  withCString "JustOneAttribute" $ \first -> withCString "testdevice" $ \second -> do
    withArray [first, second] $ \a -> do
      tango_init_server 2 a
      getterWrapped <- createGetterWrapper attributeGetter
      setterWrapped <- createSetterWrapper attributeSetter
      tango_set_attribute_getter getterWrapped
      tango_set_attribute_setter setterWrapped
      tango_start_server
