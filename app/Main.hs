{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM_, void, when)
import qualified Data.Vector.Storable as V
import Foreign
  ( Storable (peek),
    alloca,
  )
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (hPutStrLn, stderr)
import Tango
  ( HaskellAttributeData (HaskellAttributeData),
    HaskellAttributeDataList (..),
    HaskellAttributeInfoList (..),
    HaskellCommandData (HaskellCommandData),
    HaskellCommandInfoList (commandInfos),
    HaskellDataFormat (..),
    HaskellDataQuality (..),
    HaskellDevFailed (..),
    HaskellDevSource (..),
    HaskellErrorStack (..),
    HaskellTangoAttributeData (HaskellAttributeDataStringArray),
    HaskellTangoCommandData (HaskellCommandCString, HaskellCommandDouble),
    HaskellTangoDataType (HaskellDevDouble, HaskellDevString),
    HaskellTangoVarArray (HaskellTangoVarArray, varArrayValues),
    Timeval (Timeval),
    devSourceFromInt,
    devSourceToInt,
    newDoubleArray,
    stringToVector,
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

checkResult :: IO (Ptr HaskellErrorStack) -> IO ()
checkResult action = do
  es <- action
  when (es /= nullPtr) $ do
    errorStack <- peek es
    stackItems <- peekArray (fromIntegral (errorStackLength errorStack)) (errorStackSequence errorStack)
    hPutStrLn stderr ("peeked " <> show (errorStackLength errorStack) <> " stack items")
    -- TODO: This can be simplified by using the Traversable instance of HaskellDevFailed to go from CString to String
    let formatDevFailed :: HaskellDevFailed CString -> IO String
        formatDevFailed (HaskellDevFailed desc reason origin _severity) = do
          desc' <- peekCString desc
          reason' <- peekCString reason
          origin' <- peekCString origin
          hPutStrLn stderr ("description: " <> show desc' <> ", reason: " <> reason' <> ", origin: " <> origin')
          pure ("description: " <> show desc' <> ", reason: " <> reason' <> ", origin: " <> origin')
    errorLines <- traverse formatDevFailed stackItems
    fail ("error in result: " <> unlines errorLines)

main :: IO ()
main = do
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
      forM_ (V.toList (commandInfos commandInfoList)) $ \ci -> do
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
    attributeNames <- alloca $ \stringArrayPtr -> do
      checkResult (tango_get_attribute_list proxyPtr stringArrayPtr)
      stringArray <- peek stringArrayPtr
      putStrLn ("<= attribute list retrieved, starting list:")
      haskellStrings <- traverse peekCString (V.toList (varArrayValues stringArray))
      forM_ haskellStrings (\str -> putStrLn ("  -> list element: " <> str))
      putStrLn "=> finished listing"
      tango_free_VarStringArray stringArrayPtr
      pure haskellStrings

    putStrLn ("=> reading all attributes")

    -- uchar_scalar is not supported by c_tango right now
    forM_ (filter (not . (`elem` ["no_value", "throw_exception", "uchar_scalar", "uchar_spectrum", "uchar_spectrum_ro", "uchar_image", "uchar_image_ro", "enum_image_ro"])) attributeNames) $ \attributeNameHaskell -> do
      withCString attributeNameHaskell $ \attributeName -> do
        putStrLn ("  <= reading " <> attributeNameHaskell)
        alloca $ \argoutPtr -> do
          checkResult (tango_read_attribute proxyPtr attributeName argoutPtr)
          argout <- peek argoutPtr
          putStrLn ("  => result " <> show argout)
          tango_free_AttributeData argoutPtr

    -- hPutStrLn stderr ("attribute names: " <> show attributeNames)
    -- bracket (traverse newCString attributeNames) (traverse free) $ \attributeNamesCStrings ->
    --   with (HaskellTangoVarArray (V.fromList attributeNamesCStrings)) $ \stringArrayPtr -> alloca $ \attributeInfoListPtr -> do
    --     checkResult (tango_get_attribute_config proxyPtr stringArrayPtr attributeInfoListPtr)
    --     stringArray <- peek stringArrayPtr
    --     hPutStrLn stderr "begin attribute config list, names"
    --     forM_ (V.toList (varArrayValues stringArray)) $ \cstring -> do
    --       str <- peekCString cstring
    --       hPutStrLn stderr ("attribute config list: " <> str)
    --     hPutStrLn stderr "end attribute config list, names"
    --     infos <- peek attributeInfoListPtr
    --     hPutStrLn stderr "begin attribute config list, attributes"
    --     forM_ (V.toList (attributeInfos infos)) $ \info -> do
    --       hPutStrLn stderr ("attribute config info: " <> show info)
    --     hPutStrLn stderr "end attribute config list, attributes"

    --   with (HaskellAttributeData HaskellScalar HaskellValid 0 (stringToVector "double_scalar") 1 0 (Timeval 0 0) HaskellDevDouble (newDoubleArray [1338.0])) $ \argoutPtr -> do
    --     hPutStrLn stderr "writing attribute"
    --     attrWriteResult <- tango_write_attribute proxyPtr argoutPtr
    --     _ <- peek argoutPtr
    --     putStrLn ("read attribute " <> show attrWriteResult)

    -- let attributesToQuery :: [String]
    --     attributesToQuery = ["double_scalar"]

    -- bracket (traverse newCString attributesToQuery) (traverse free) $ \attributeNamesCStrings ->
    --   with (HaskellTangoVarArray (V.fromList attributeNamesCStrings)) $ \attributeNames -> alloca $ \dataListPtr -> do
    --     checkResult (tango_read_attributes proxyPtr attributeNames dataListPtr)
    --     hPutStrLn stderr "begin attribute data list"
    --     dataList <- peek dataListPtr
    --     forM_ (V.toList (attributeDatas dataList)) $ \data' -> do
    --       hPutStrLn stderr ("attribute data info: " <> show data')
    --     hPutStrLn stderr "end attribute data list"

    -- alloca $ \dbProxyPtr -> do
    --   checkResult (tango_create_database_proxy dbProxyPtr)

    --   dbProxy <- peek dbProxyPtr
    --   alloca $ \dbDatumPtr -> do
    --     withCString "*" $ \filterStr -> do
    --       hPutStrLn stderr "getting db datum"
    --       checkResult (tango_get_device_exported dbProxy filterStr dbDatumPtr)
    --       dbDatum <- peek dbDatumPtr
    --       hPutStrLn stderr ("db datum for name filter: " <> show dbDatum)
    --       checkResult (tango_get_device_exported_for_class dbProxy filterStr dbDatumPtr)
    --       dbDatum' <- peek dbDatumPtr
    --       hPutStrLn stderr ("db datum for class filter: " <> show dbDatum')
    --       checkResult (tango_get_object_list dbProxy filterStr dbDatumPtr)
    --       dbDatum'' <- peek dbDatumPtr
    --       hPutStrLn stderr ("db get object list: " <> show dbDatum'')
    --       checkResult (tango_get_object_property_list dbProxy filterStr filterStr dbDatumPtr)
    --       dbDatum''' <- peek dbDatumPtr
    --       hPutStrLn stderr ("db get object property list: " <> show dbDatum''')

    --       alloca $ \dbDataPtr -> do
    --         checkResult (tango_get_property dbProxy filterStr dbDataPtr)
    --         dbData <- peek dbDataPtr
    --         hPutStrLn stderr ("db get property data: " <> show dbData)

    --   checkResult (tango_delete_database_proxy dbProxy)

    void (tango_delete_device_proxy proxyPtr)
