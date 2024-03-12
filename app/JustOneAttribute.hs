{-# LANGUAGE BlockArguments #-}

import Foreign.C (CLong)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Array (withArray)
import Tango
  ( HaskellAttrWriteType (ReadWrite),
    HaskellAttributeDefinition (..),
    HaskellTangoDataType (HaskellDevLong64),
    createGetterWrapper,
    createSetterWrapper,
    tango_add_attribute_definition,
    tango_init_server,
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
  withCString "JustOneAttribute" \first -> withCString "testdevice" \second -> do
    withArray [first, second] \a -> do
      withCString "first_attribute" \firstAttributeName -> withCString "second_attribute" \secondAttributeName -> do
        getterWrapped <- createGetterWrapper attributeGetter
        setterWrapped <- createSetterWrapper attributeSetter
        with (HaskellAttributeDefinition firstAttributeName HaskellDevLong64 ReadWrite setterWrapped getterWrapped) \attributeDefinition -> do
          tango_add_attribute_definition attributeDefinition
        with (HaskellAttributeDefinition secondAttributeName HaskellDevLong64 ReadWrite setterWrapped getterWrapped) \attributeDefinition -> do
          tango_add_attribute_definition attributeDefinition
        tango_init_server 2 a
        tango_start_server
