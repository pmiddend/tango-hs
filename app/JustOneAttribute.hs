{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Foreign.C (CInt, CLong)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Tango.Common
  ( HaskellAttrWriteType (Read, ReadWrite),
    HaskellTangoDataType (HaskellDevBoolean, HaskellDevLong64, HaskellDevString, HaskellDevVoid),
    HaskellTangoDevState (Moving),
  )
import Tango.Server
  ( HaskellAttributeDefinition (..),
    HaskellCommandDefinition (..),
    createCommandCallback,
    createFnWrapper,
    createGlobalFinalizer,
    tango_server_add_attribute_definition,
    tango_server_add_command_definition,
    tango_server_init,
    tango_server_set_status,
    tango_server_start,
  )

attributeStringGetter :: Ptr () -> IO ()
attributeStringGetter ptr = do
  cstr <- newCString "test string 123"
  poke (castPtr ptr) cstr

attributeStringSetter :: Ptr () -> IO ()
attributeStringSetter ptr = do
  putStrLn "in setter"
  cstr :: CString <- peek (castPtr ptr)
  putStrLn "peeked"
  mycstring <- peekCString cstr
  putStrLn $ "string was |" <> mycstring <> "|"

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

testCommandVoidVoid :: Ptr () -> IO (Ptr ())
testCommandVoidVoid _ = do
  putStrLn "in test command void void"
  pure nullPtr

testCommandVoidLong64 :: Ptr () -> IO (Ptr ())
testCommandVoidLong64 _ = do
  putStrLn "in test command void long64"
  castPtr <$> new (1337 :: CLong)

testCommandLong64Void :: Ptr () -> IO (Ptr ())
testCommandLong64Void ptr = do
  long64 :: CLong <- peek (castPtr ptr)
  putStrLn ("in test command long void, argument " <> show long64)
  pure nullPtr

testCommandStringVoid :: Ptr () -> IO (Ptr ())
testCommandStringVoid ptr = do
  let arg :: CString
      arg = castPtr ptr
  str <- peekCString arg
  putStrLn ("in test command string void, argument " <> str)
  pure nullPtr

testCommandVoidString :: Ptr () -> IO (Ptr ())
testCommandVoidString _ = castPtr <$> newCString "hehe 1337"

main :: IO ()
main = do
  freeFinalizerWrapped <- createGlobalFinalizer free
  withCString "JustOneAttribute" \first -> withCString "testdevice" \second -> do
    withArray [first, second] \a -> do
      withCString "long_attribute" \longAttributeName -> withCString "string_attribute" \stringAttributeName -> do
        longGetterWrapped <- createFnWrapper attributeLongGetter
        longSetterWrapped <- createFnWrapper attributeLongSetter
        with (HaskellAttributeDefinition longAttributeName HaskellDevLong64 ReadWrite longSetterWrapped longGetterWrapped) \attributeDefinition -> do
          tango_server_add_attribute_definition attributeDefinition
        stringGetterWrapped <- createFnWrapper attributeStringGetter
        stringSetterWrapped <- createFnWrapper attributeStringSetter
        with (HaskellAttributeDefinition stringAttributeName HaskellDevString ReadWrite stringSetterWrapped stringGetterWrapped) \attributeDefinition -> do
          tango_server_add_attribute_definition attributeDefinition
        withCString "test_command_void_void" \command_name -> do
          commandCallback <- createCommandCallback testCommandVoidVoid
          with (HaskellCommandDefinition command_name HaskellDevVoid HaskellDevVoid commandCallback) \commandDefinition -> do
            tango_server_add_command_definition commandDefinition
        withCString "test_command_long64_void" \command_name -> do
          commandCallback <- createCommandCallback testCommandLong64Void
          with (HaskellCommandDefinition command_name HaskellDevLong64 HaskellDevVoid commandCallback) \commandDefinition -> do
            tango_server_add_command_definition commandDefinition
        withCString "test_command_string_void" \command_name -> do
          commandCallback <- createCommandCallback testCommandStringVoid
          with (HaskellCommandDefinition command_name HaskellDevString HaskellDevVoid commandCallback) \commandDefinition -> do
            tango_server_add_command_definition commandDefinition
        withCString "test_command_void_string" \command_name -> do
          commandCallback <- createCommandCallback testCommandVoidString
          with (HaskellCommandDefinition command_name HaskellDevVoid HaskellDevString commandCallback) \commandDefinition -> do
            tango_server_add_command_definition commandDefinition
        withCString "test_command_void_long64" \command_name -> do
          commandCallback <- createCommandCallback testCommandVoidLong64
          with (HaskellCommandDefinition command_name HaskellDevVoid HaskellDevLong64 commandCallback) \commandDefinition -> do
            tango_server_add_command_definition commandDefinition
        withCString "initial status hihihi" \initialStatus -> do
          tango_server_init 2 a freeFinalizerWrapped initialStatus (fromIntegral $ fromEnum $ Moving)
          forkIO do
            putStrLn "sleeping"
            threadDelay (5 * 1000 * 1000)
            putStrLn "setting status"
            withCString "new status" tango_server_set_status
            putStrLn "setting status done"
          tango_server_start
