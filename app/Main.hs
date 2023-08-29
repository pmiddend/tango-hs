{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.Text (unpack)
import Foreign
  ( Storable (peek),
    alloca,
  )
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (hPutStrLn, stderr)
import Tango
  ( DeviceProxyPtr,
    HaskellAttributeData (HaskellAttributeData),
    HaskellAttributeDataList (..),
    HaskellAttributeInfoList (..),
    HaskellCommandData (HaskellCommandData),
    HaskellCommandInfoList (commandInfoLength, commandInfoSequence),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDevFailed (..),
    HaskellDevSource (..),
    HaskellErrorStack (..),
    HaskellTangoAttributeData (..),
    HaskellTangoCommandData (HaskellCommandCString, HaskellCommandDouble),
    HaskellTangoDataType (HaskellDevDouble, HaskellDevString),
    HaskellTangoVarArray (HaskellTangoVarArray, varArrayLength, varArrayValues),
    Timeval (Timeval),
    devSourceFromInt,
    devSourceToInt,
    tangoCommandData,
    tango_command_inout,
    tango_command_list_query,
    tango_create_database_proxy,
    tango_create_device_proxy,
    tango_delete_database_proxy,
    tango_delete_device_proxy,
    tango_free_AttributeData,
    tango_free_CommandData,
    tango_free_CommandInfoList,
    tango_free_VarStringArray,
    tango_get_attribute_config,
    tango_get_attribute_list,
    tango_get_device_exported,
    tango_get_device_exported_for_class,
    tango_get_object_list,
    tango_get_object_property_list,
    tango_get_property,
    tango_get_source,
    tango_get_timeout_millis,
    tango_is_locked,
    tango_is_locked_by_me,
    tango_lock,
    tango_locking_status,
    tango_read_attribute,
    tango_read_attributes,
    tango_set_source,
    tango_set_timeout_millis,
    tango_unlock,
    tango_write_attribute,
  )
import TangoHL (getTimeoutMillis, setTimeoutMillis, withDeviceProxy)

checkResult :: IO (Ptr HaskellErrorStack) -> IO ()
checkResult action = do
  es <- action
  when (es /= nullPtr) $ do
    errorStack <- peek es
    stackItems <- peekArray (fromIntegral (errorStackLength errorStack)) (errorStackSequence errorStack)
    -- TODO: This can be simplified by using the Traversable instance of HaskellDevFailed to go from CString to String
    let formatDevFailed :: HaskellDevFailed CString -> IO String
        formatDevFailed (HaskellDevFailed desc reason origin _severity) = do
          desc' <- peekCString desc
          reason' <- peekCString reason
          origin' <- peekCString origin
          pure ("description: " <> show desc' <> ", reason: " <> reason' <> ", origin: " <> origin')
    errorLines <- traverse formatDevFailed stackItems
    fail ("error in result: " <> unlines errorLines)

main = withDeviceProxy "sys/tg_test/1" $ \proxyPtr -> do
  setTimeoutMillis proxyPtr 1337
  millis <- getTimeoutMillis proxyPtr
  putStrLn ("millis: " <> show millis)

main2 :: IO ()
main2 = do
  let proxyAddress = "sys/tg_test/1"
  putStrLn ("<= creating proxy for " <> proxyAddress)
  alloca $ \proxyPtrPtr -> do
    proxyPtr <- withCString proxyAddress $ \proxyName -> do
      checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
      proxyPtr' <- peek proxyPtrPtr
      putStrLn "=> proxy received successfully!"
      pure proxyPtr'

    let desiredTimeout = 1337
    putStrLn ("<= setting timeout to " <> show desiredTimeout <> "ms")
    checkResult (tango_set_timeout_millis proxyPtr desiredTimeout)
    alloca $ \millisPtr -> do
      checkResult (tango_get_timeout_millis proxyPtr millisPtr)
      putStrLn "=> setting timeout done"
      putStrLn "<= now getting the timeout"
      millis <- peek millisPtr
      putStrLn ("=> timeout is " <> show millis)
      when (millis /= desiredTimeout) (error "timeout wasn't the desired one")

    putStrLn "<= locking the device"
    checkResult (tango_lock proxyPtr)
    putStrLn "=> locked"

    alloca $ \boolPtr -> do
      putStrLn "<= checking lock status"
      checkResult (tango_is_locked proxyPtr boolPtr)
      lockStatus <- peek boolPtr
      putStrLn ("=> lock status: " <> show lockStatus)
      when (not lockStatus) (error "we locked the device and then asked for the lock status and it was false - how can that be?")
      putStrLn "<= locked by me?"
      checkResult (tango_is_locked_by_me proxyPtr boolPtr)
      lockedByMe <- peek boolPtr
      putStrLn ("=> locked by me: " <> show lockedByMe)
      when (not lockedByMe) (error "we locked the device and then asked if _we_ locked it and it was false - how can that be?")

    putStrLn "<= unlocking"
    checkResult (tango_unlock proxyPtr)

    alloca $ \boolPtr -> do
      putStrLn "<= checking lock status"
      checkResult (tango_is_locked proxyPtr boolPtr)
      lockStatus <- peek boolPtr
      putStrLn ("=> lock status: " <> show lockStatus)
      when lockStatus (error "we unlocked the device and then asked for the lock status and it was true - how can that be?")

    putStrLn "<= retrieving string locking status"
    alloca $ \strPtrPtr -> do
      checkResult (tango_locking_status proxyPtr strPtrPtr)
      strPtr <- peek strPtrPtr
      haskellStr <- peekCString strPtr
      putStrLn ("=> string locking status: " <> haskellStr)
      free strPtr

    putStrLn "<= setting source to \"Dev\""
    checkResult (tango_set_source proxyPtr (devSourceToInt Dev))
    putStrLn "=> source set"

    putStrLn "<= getting source"
    alloca $ \sourcePtr -> do
      checkResult (tango_get_source proxyPtr sourcePtr)
      source <- peek sourcePtr
      putStrLn ("=> source is " <> show (devSourceFromInt source))

    putStrLn "<= listing commands"
    alloca $ \commandInfoListPtr -> do
      checkResult (tango_command_list_query proxyPtr commandInfoListPtr)
      commandInfoList <- peek commandInfoListPtr
      putStrLn "=> list retrieved, starting list:"
      commandInfoArray <- peekArray (fromIntegral (commandInfoLength commandInfoList)) (commandInfoSequence commandInfoList)
      forM_ commandInfoArray $ \ci -> do
        hPutStrLn stderr ("  -> info list element: " <> show ci)
      putStrLn "=> finished listing"
      tango_free_CommandInfoList commandInfoListPtr

    let doubleCommandName = "DevDouble"

    putStrLn ("<= executing double command " <> doubleCommandName)
    with (HaskellCommandData HaskellDevDouble (HaskellCommandDouble 3.0)) $ \arginPtr ->
      alloca $ \argoutPtr -> withCString doubleCommandName $ \cmdName -> do
        checkResult (tango_command_inout proxyPtr cmdName arginPtr argoutPtr)
        argOut <- peek argoutPtr
        putStrLn ("<= executed command " <> doubleCommandName <> ", result " <> show argOut)
        tango_free_CommandData argoutPtr

    let stringCommandName = "DevString"
        stringCommandInput = "foobar"

    putStrLn ("=> executing string command " <> stringCommandName)
    withCString stringCommandInput $ \cmdInput -> with (HaskellCommandData HaskellDevString (HaskellCommandCString cmdInput)) $ \arginPtr ->
      alloca $ \argoutPtr -> withCString "DevString" $ \cmdName -> do
        checkResult (tango_command_inout proxyPtr cmdName arginPtr argoutPtr)
        argOut <- peek argoutPtr
        putStrLn ("<= executed command " <> stringCommandName <> ", result " <> show argOut)
        putStrLn "=> decoding output"
        case tangoCommandData argOut of
          HaskellCommandCString s -> do
            decoded <- peekCString s
            putStrLn ("<= output decoded: " <> decoded)
          _ -> error "<= couldn't decode output, type unexpected"

    putStrLn ("=> retrieving attribute list")
    attributeNames :: [String] <- alloca $ \stringArrayPtr -> do
      checkResult (tango_get_attribute_list proxyPtr stringArrayPtr)
      stringArray <- peek stringArrayPtr
      putStrLn ("<= attribute list retrieved, starting list:")
      cStringList <- peekArray (fromIntegral (varArrayLength stringArray)) (varArrayValues stringArray)
      haskellStrings <- traverse peekCString cStringList
      forM_ haskellStrings (\str -> putStrLn ("  -> list element: " <> str))
      putStrLn "=> finished listing"
      tango_free_VarStringArray stringArrayPtr
      pure haskellStrings

    -- putStrLn ("=> reading all attributes")

    -- forM_ (filter (not . (`elem` ["no_value", "throw_exception"])) attributeNames) $ \attributeNameHaskell -> do
    --   withCString attributeNameHaskell $ \attributeName -> do
    --     putStrLn ("  <= reading " <> attributeNameHaskell)
    --     alloca $ \argoutPtr -> do
    --       checkResult (tango_read_attribute proxyPtr attributeName argoutPtr)
    --       argout <- peek argoutPtr
    --       putStrLn ("  => result " <> show argout)
    --       tango_free_AttributeData argoutPtr

    -- putStrLn ("<= read all attributes")

    putStrLn ("=> reading attribute configurations")
    bracket (traverse newCString attributeNames) (traverse free) $ \attributeNamesCStrings ->
      withArray attributeNamesCStrings $ \attributeNamesCStringPtr ->
        with (HaskellTangoVarArray (fromIntegral (length attributeNames)) attributeNamesCStringPtr) $ \stringArrayPtr -> alloca $ \attributeInfoListPtr -> do
          checkResult (tango_get_attribute_config proxyPtr stringArrayPtr attributeInfoListPtr)
          infos <- peek attributeInfoListPtr
          putStrLn ("<= peeked attribute info list, now peeking each element")
          infosList <- peekArray (fromIntegral (attributeInfoListLength infos)) (attributeInfoListSequence infos)
          putStrLn ("<= read attribute configurations, starting list of attribute configurations:")
          forM_ infosList $ \info -> do
            putStrLn ("  -> list element: " <> show info)
          putStrLn ("=> finish listing configurations")

    withCString "double_scalar" $ \nameCString -> withArray [1338.0] $ \doubleArrayPtr ->
      with
        ( HaskellAttributeData
            HaskellScalar
            HaskellValid
            0
            nameCString
            1
            0
            (Timeval 0 0)
            HaskellDevDouble
            (HaskellAttributeDataDoubleArray (HaskellTangoVarArray 1 doubleArrayPtr))
        )
        $ \argoutPtr -> do
          putStrLn "=> writing attribute \"double_scalar\""
          checkResult (tango_write_attribute proxyPtr argoutPtr)
          output <- peek argoutPtr
          putStrLn ("<= read attribute " <> show output)

    let attributesToQuery :: [String]
        attributesToQuery = filter (not . (`elem` ["no_value", "throw_exception"])) attributeNames

    putStrLn "<= retrieving attribute values"
    bracket (traverse newCString attributesToQuery) (traverse free) $ \attributeNamesCStrings ->
      withArray attributeNamesCStrings $ \attributeNamesCStringPtr ->
        with (HaskellTangoVarArray (fromIntegral (length attributesToQuery)) attributeNamesCStringPtr) $ \stringArrayPtr -> alloca $ \dataListPtr -> do
          checkResult (tango_read_attributes proxyPtr stringArrayPtr dataListPtr)
          putStrLn "=> printing attribute values"
          dataList <- peek dataListPtr
          putStrLn "=> peeked data list ptr, now peeking every element"
          datas <- peekArray (fromIntegral (attributeDataListLength dataList)) (attributeDataListSequence dataList)
          forM_ datas $ \data' -> do
            putStrLn ("=> attribute data info: " <> show data')
          putStrLn "=> finish printing attribute values"

    alloca $ \dbProxyPtr -> do
      checkResult (tango_create_database_proxy dbProxyPtr)

      dbProxy <- peek dbProxyPtr
      alloca $ \dbDatumPtr -> do
        withCString "*" $ \filterStr -> do
          putStrLn "<= getting db datum"
          checkResult (tango_get_device_exported dbProxy filterStr dbDatumPtr)
          dbDatum <- peek dbDatumPtr
          putStrLn ("=> db datum for name filter: " <> show dbDatum)
          checkResult (tango_get_device_exported_for_class dbProxy filterStr dbDatumPtr)
          dbDatum' <- peek dbDatumPtr
          putStrLn ("<= db datum for class filter: " <> show dbDatum')
          checkResult (tango_get_object_list dbProxy filterStr dbDatumPtr)
          dbDatum'' <- peek dbDatumPtr
          putStrLn ("=> db get object list: " <> show dbDatum'')
          checkResult (tango_get_object_property_list dbProxy filterStr filterStr dbDatumPtr)
          dbDatum''' <- peek dbDatumPtr
          putStrLn ("<= db get object property list: " <> show dbDatum''')

          alloca $ \dbDataPtr -> do
            checkResult (tango_get_property dbProxy filterStr dbDataPtr)
            dbData <- peek dbDataPtr
            putStrLn ("=> db get property data: " <> show dbData)

      checkResult (tango_delete_database_proxy dbProxy)

    void (tango_delete_device_proxy proxyPtr)
