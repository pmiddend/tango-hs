{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import qualified Data.Vector.Storable as V
import Foreign
  ( Storable (peek),
    alloca,
  )
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.Marshal (free, peekArray, with)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
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
    HaskellTangoAttributeData (HaskellStringArray),
    HaskellTangoCommandData (HaskellCommandDouble, HaskellCommandString),
    HaskellTangoDataType (HaskellDevDouble, HaskellDevString),
    HaskellVarStringArray (HaskellVarStringArray, strings),
    Timeval (Timeval),
    devSourceToInt,
    newDoubleArray,
    stringToVector,
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
        formatDevFailed (HaskellDevFailed desc reason origin severity) = do
          desc' <- peekCString desc
          reason' <- peekCString reason
          origin' <- peekCString origin
          hPutStrLn stderr ("description: " <> show desc' <> ", reason: " <> reason' <> ", origin: " <> origin')
          pure ("description: " <> show desc' <> ", reason: " <> reason' <> ", origin: " <> origin')
    errorLines <- traverse formatDevFailed stackItems
    fail ("error in result: " <> unlines errorLines)

main :: IO ()
main = do
  putStrLn "creating proxy"
  alloca $ \proxyPtrPtr -> withCString "sys/tg_test/1" $ \proxyName -> do
    checkResult (tango_create_device_proxy proxyName proxyPtrPtr)
    proxyPtr <- peek proxyPtrPtr
    putStrLn "got proxy"

    putStrLn "setting timeout to 1337ms"
    checkResult (tango_set_timeout_millis proxyPtr 1337)
    alloca $ \millisPtr -> do
      checkResult (tango_get_timeout_millis proxyPtr millisPtr)
      millis <- peek millisPtr
      putStrLn ("millis are " <> show millis)

    putStrLn "locking"
    checkResult (tango_lock proxyPtr)

    putStrLn "locked?"
    alloca $ \boolPtr -> do
      checkResult (tango_is_locked proxyPtr boolPtr)
      bool <- peek boolPtr
      print bool
      putStrLn "locked by me?"
      checkResult (tango_is_locked_by_me proxyPtr boolPtr)
      bool' <- peek boolPtr
      print bool'

    putStrLn "unlocking"
    checkResult (tango_unlock proxyPtr)

    putStrLn "locked after unlocking?"
    alloca $ \boolPtr -> do
      checkResult (tango_is_locked proxyPtr boolPtr)
      bool <- peek boolPtr
      print bool

    putStrLn "retrieving locking status"
    alloca $ \strPtrPtr -> do
      checkResult (tango_locking_status proxyPtr strPtrPtr)
      strPtr <- peek strPtrPtr
      haskellStr <- peekCString strPtr
      putStrLn ("status: " <> haskellStr)
      free strPtr

    putStrLn "setting source"
    checkResult (tango_set_source proxyPtr (devSourceToInt Dev))

    putStrLn "getting source"
    alloca $ \sourcePtr -> do
      checkResult (tango_get_source proxyPtr sourcePtr)
      source <- peek sourcePtr
      putStrLn ("source is " <> show source)

    putStrLn "listing commands"
    alloca $ \commandInfoListPtr -> do
      checkResult (tango_command_list_query proxyPtr commandInfoListPtr)
      commandInfoList <- peek commandInfoListPtr
      forM_ (V.toList (commandInfos commandInfoList)) $ \ci -> do
        hPutStrLn stderr ("info list element: " <> show ci)
      tango_free_CommandInfoList commandInfoListPtr

    with (HaskellCommandData HaskellDevDouble (HaskellCommandDouble 3.0)) $ \arginPtr ->
      alloca $ \argoutPtr -> withCString "DevDouble" $ \cmdName -> do
        hPutStrLn stderr "executing command"
        commandResult <- tango_command_inout proxyPtr cmdName arginPtr argoutPtr
        hPutStrLn stderr ("executed command: " <> show commandResult)
        argOut <- peek argoutPtr
        hPutStrLn stderr ("result: " <> show argOut)
        tango_free_CommandData argoutPtr

    with (HaskellCommandData HaskellDevString (HaskellCommandString (V.fromList [115, 116, 114, 105, 110, 103, 95, 115, 99, 97, 108, 97, 114]))) $ \arginPtr ->
      alloca $ \argoutPtr -> withCString "DevString" $ \cmdName -> do
        hPutStrLn stderr "executing command"
        commandResult <- tango_command_inout proxyPtr cmdName arginPtr argoutPtr
        hPutStrLn stderr ("executed command: " <> show commandResult)
        argOut <- peek argoutPtr
        hPutStrLn stderr ("result: " <> show argOut)

    attributeNames <- alloca $ \stringArrayPtr -> do
      checkResult (tango_get_attribute_list proxyPtr stringArrayPtr)
      stringArray <- peek stringArrayPtr
      hPutStrLn stderr "begin attribute list"
      haskellStrings <- traverse peekCString (V.toList (strings stringArray))
      forM_ haskellStrings (\str -> hPutStrLn stderr ("attribute list: " <> str))
      -- forM_ (V.toList (strings stringArray)) $ \cstring -> do
      --   str <- peekCString cstring
      --   hPutStrLn stderr ("attribute list: " <> str)
      hPutStrLn stderr "end attribute list"
      tango_free_VarStringArray stringArrayPtr
      pure haskellStrings

    hPutStrLn stderr ("attribute names: " <> show attributeNames)
    bracket (traverse newCString attributeNames) (traverse free) $ \attributeNamesCStrings ->
      with (HaskellVarStringArray (V.fromList attributeNamesCStrings)) $ \stringArrayPtr -> alloca $ \attributeInfoListPtr -> do
        checkResult (tango_get_attribute_config proxyPtr stringArrayPtr attributeInfoListPtr)
        stringArray <- peek stringArrayPtr
        hPutStrLn stderr "begin attribute config list, names"
        forM_ (V.toList (strings stringArray)) $ \cstring -> do
          str <- peekCString cstring
          hPutStrLn stderr ("attribute config list: " <> str)
        hPutStrLn stderr "end attribute config list, names"
        infos <- peek attributeInfoListPtr
        hPutStrLn stderr "begin attribute config list, attributes"
        forM_ (V.toList (attributeInfos infos)) $ \info -> do
          hPutStrLn stderr ("attribute config info: " <> show info)
        hPutStrLn stderr "end attribute config list, attributes"

    withCString "double_scalar" $ \attributeName -> do
      hPutStrLn stderr "before with"
      alloca $ \argoutPtr -> do
        hPutStrLn stderr "reading attribute"
        hFlush stdout
        attrReadResult <- tango_read_attribute proxyPtr attributeName argoutPtr
        argout' <- peek argoutPtr
        putStrLn ("read attribute " <> show attrReadResult)
        putStrLn ("result " <> show argout')
        tango_free_AttributeData argoutPtr

      with (HaskellAttributeData HaskellScalar HaskellValid 0 (stringToVector "double_scalar") 1 0 (Timeval 0 0) HaskellDevDouble (newDoubleArray [1338.0])) $ \argoutPtr -> do
        hPutStrLn stderr "writing attribute"
        attrWriteResult <- tango_write_attribute proxyPtr argoutPtr
        _ <- peek argoutPtr
        putStrLn ("read attribute " <> show attrWriteResult)

    let attributesToQuery :: [String]
        attributesToQuery = ["double_scalar"]

    bracket (traverse newCString attributesToQuery) (traverse free) $ \attributeNamesCStrings ->
      with (HaskellVarStringArray (V.fromList attributeNamesCStrings)) $ \attributeNames -> alloca $ \dataListPtr -> do
        checkResult (tango_read_attributes proxyPtr attributeNames dataListPtr)
        hPutStrLn stderr "begin attribute data list"
        dataList <- peek dataListPtr
        forM_ (V.toList (attributeDatas dataList)) $ \data' -> do
          hPutStrLn stderr ("attribute data info: " <> show data')
        hPutStrLn stderr "end attribute data list"

    alloca $ \dbProxyPtr -> do
      checkResult (tango_create_database_proxy dbProxyPtr)

      dbProxy <- peek dbProxyPtr
      alloca $ \dbDatumPtr -> do
        withCString "*" $ \filterStr -> do
          hPutStrLn stderr "getting db datum"
          checkResult (tango_get_device_exported dbProxy filterStr dbDatumPtr)
          dbDatum <- peek dbDatumPtr
          hPutStrLn stderr ("db datum for name filter: " <> show dbDatum)
          checkResult (tango_get_device_exported_for_class dbProxy filterStr dbDatumPtr)
          dbDatum' <- peek dbDatumPtr
          hPutStrLn stderr ("db datum for class filter: " <> show dbDatum')
          checkResult (tango_get_object_list dbProxy filterStr dbDatumPtr)
          dbDatum'' <- peek dbDatumPtr
          hPutStrLn stderr ("db get object list: " <> show dbDatum'')
          checkResult (tango_get_object_property_list dbProxy filterStr filterStr dbDatumPtr)
          dbDatum''' <- peek dbDatumPtr
          hPutStrLn stderr ("db get object property list: " <> show dbDatum''')

      checkResult (tango_delete_database_proxy dbProxy)

    void (tango_delete_device_proxy proxyPtr)
