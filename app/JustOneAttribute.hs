{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Foreign.C (CLong)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, poke)
import Tango
  ( HaskellAttrWriteType (ReadWrite),
    HaskellAttributeDefinition (..),
    HaskellTangoDataType (HaskellDevBoolean, HaskellDevLong64),
    createGetterWrapper,
    createSetterWrapper,
    tango_add_attribute_definition,
    tango_init_server,
    tango_start_server,
  )

attributeLongGetter :: Ptr () -> IO ()
attributeLongGetter ptr = do
  putStrLn "reading long"
  poke (castPtr ptr) (1339 :: CLong)

attributeLongSetter :: Ptr () -> IO ()
attributeLongSetter ptr = do
  newValue :: CLong <- peek (castPtr ptr)
  putStrLn $ "new long value " <> show newValue

attributeBoolGetter :: Ptr () -> IO ()
attributeBoolGetter ptr = do
  putStrLn "reading bool"
  poke (castPtr ptr) True

attributeBoolSetter :: Ptr () -> IO ()
attributeBoolSetter ptr = do
  newValue :: Bool <- peek (castPtr ptr)
  putStrLn $ "new bool value " <> show newValue

main :: IO ()
main = do
  withCString "JustOneAttribute" \first -> withCString "testdevice" \second -> do
    withArray [first, second] \a -> do
      withCString "long_attribute" \longAttributeName -> withCString "bool_attribute" \boolAttributeName -> do
        longGetterWrapped <- createGetterWrapper attributeLongGetter
        longSetterWrapped <- createSetterWrapper attributeLongSetter
        with (HaskellAttributeDefinition longAttributeName HaskellDevLong64 ReadWrite longSetterWrapped longGetterWrapped) \attributeDefinition -> do
          tango_add_attribute_definition attributeDefinition
        boolGetterWrapped <- createGetterWrapper attributeBoolGetter
        boolSetterWrapped <- createSetterWrapper attributeBoolSetter
        with (HaskellAttributeDefinition boolAttributeName HaskellDevBoolean ReadWrite boolSetterWrapped boolGetterWrapped) \attributeDefinition -> do
          tango_add_attribute_definition attributeDefinition
        tango_init_server 2 a
        tango_start_server
