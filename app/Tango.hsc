{-# LANGUAGE CPP                         #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE BangPatterns                         #-}
{-# LANGUAGE DeriveFunctor                         #-}
{-# LANGUAGE DeriveFoldable                         #-}
{-# LANGUAGE DeriveTraversable                         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tango(tango_create_device_proxy,
             tango_delete_device_proxy,
             tango_read_attribute,
             tango_write_attribute,
             tango_command_inout,
             tango_free_AttributeData,
             tango_free_CommandData,
             tango_set_timeout_millis,
             tango_create_database_proxy,
             tango_delete_database_proxy,
             tango_get_object_list,
             tango_get_object_property_list,
             tango_free_VarStringArray,
             tango_get_attribute_list,
             tango_read_attributes,
             tango_get_device_exported,
             HaskellDataFormat(..),
             HaskellVarStringArray(..),
             HaskellDataQuality(..),
             HaskellAttributeInfoList(..),
             HaskellAttributeDataList(..),
             Timeval(..),
             tango_get_timeout_millis,
             tango_set_source,
             tango_command_list_query,
             tango_free_CommandInfoList,
             tango_get_source,
             tango_lock,
             tango_get_device_exported_for_class,
             haskellDisplayLevelExpert,
             tango_get_attribute_config,
             haskellDisplayLevelOperator,
             tango_unlock,
             tango_is_locked,
             tango_locking_status,
             tango_is_locked_by_me,
             DeviceProxyPtr,
             HaskellDevSource(..),
             HaskellErrorStack(..),
             HaskellDevFailed(..),
             HaskellAttributeData(..),
             HaskellCommandData(..),
             HaskellTangoDataType(HaskellDevBoolean, HaskellDevDouble, HaskellDevString),
             HaskellTangoCommandData(HaskellCommandDouble, HaskellCommandString),
             HaskellCommandInfoList(..),
             HaskellTangoAttributeData(..),
             devSourceToInt,
             newDoubleArray,
             stringToVector
             ) where

import Foreign(Storable(peek, poke, alignment, sizeOf), pokeByteOff, peekArray, peekByteOff)
import Foreign.C.String(peekCString, CString, castCharToCChar)
import Foreign.C.Types(CULong, CBool, CDouble, CInt, CLong, CChar, CInt(CInt))
import Data.Word(Word32)
import Data.Int(Int32)
import Foreign.Ptr(Ptr, castPtr)
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector.Storable as V
import Data.List(find)

#include <c_tango.h>

peekBounded :: (Show a, Eq a, Enum a, Bounded a) => String -> Ptr a -> IO a
peekBounded desc ptr = do
    value :: CInt <- peek (castPtr ptr)
    case snd <$> find ((==value) . fst) (zip [0..] [minBound..maxBound]) of
      Nothing -> error ("invalid constant (" <> desc <> "): " <> show value)
      Just v -> pure v

pokeBounded :: (Show a, Eq a, Enum a, Bounded a) => String -> Ptr a -> a -> IO ()
pokeBounded desc ptr x =
    case fst <$> (find ((==x) . snd) (zip [0..] [minBound..maxBound])) of
      Nothing -> error ("invalid constant (" <> desc <> "): " <> show x)
      Just v -> poke @CInt (castPtr ptr) v


-- Taken from https://github.com/ifesdjeen/haskell-ffi-tutorial/blob/d93f5354177ec69fcc937803695d07c3b8121bd3/src/Example.hsc
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data HaskellDevSource = Dev | Cache | CacheDev deriving(Show, Eq, Bounded, Enum)

devSourceToInt :: HaskellDevSource -> CInt
devSourceToInt Dev = 0
devSourceToInt Cache = 1
devSourceToInt CacheDev = 2
  
data HaskellTangoCommandData = HaskellCommandDouble !CDouble
                             | HaskellCommandString !(V.Vector CChar)
                               deriving(Show)

data HaskellTangoAttributeData = HaskellDoubleArray !HaskellVarDoubleArray
                               | HaskellBoolArray !HaskellVarBoolArray
                               | HaskellStringArray !HaskellVarStringArray
                               deriving(Show)

data HaskellTangoPropertyData =  HaskellPropDoubleArray !HaskellVarDoubleArray
                               | HaskellPropBoolArray !HaskellVarBoolArray
                               | HaskellPropStringArray !HaskellVarStringArray
                               deriving(Show)

newDoubleArray :: [CDouble] -> HaskellTangoAttributeData
newDoubleArray = HaskellDoubleArray . HaskellVarDoubleArray . V.fromList

-- withStringArray :: [String] -> (V.Vector CString -> IO b) -> IO b
-- withStringArray strings =
--   let retrieveStrings = V.fromList <$> traverse newCString strings
--       destroyStrings = traverse free . V.toList
--   in bracket retrieveStrings destroyStrings

data HaskellTangoDataType = HaskellDevVoid
                          | HaskellDevBoolean
                          | HaskellDevShort
                          | HaskellDevLong
                          | HaskellDevFloat
                          | HaskellDevDouble
                          | HaskellDevUShort
                          | HaskellDevULong
                          | HaskellDevString
                          | HaskellDevVarCharArray
                          | HaskellDevVarShortArray
                          | HaskellDevVarLongArray
                          | HaskellDevVarFloatArray
                          | HaskellDevVarDoubleArray
                          | HaskellDevVarUShortArray
                          | HaskellDevVarULongArray
                          | HaskellDevVarStringArray
                          | HaskellDevVarLongStringArray
                          | HaskellDevVarDoubleStringArray
                          | HaskellDevState
                          | HaskellConstDevString
                          | HaskellDevVarBooleanArray
                          | HaskellDevUChar
                          | HaskellDevLong64
                          | HaskellDevULong64
                          | HaskellDevVarLong64Array
                          | HaskellDevVarULong64Array
                          | HaskellDevInt
                          | HaskellDevEncoded
                          -- We explicitly have a type with index 29 and I don't know what that's supposed to be
                          | HaskellDevUnknown
                          deriving(Show, Eq, Ord, Bounded, Enum)


instance Storable HaskellTangoDataType where
  sizeOf _ = (#size TangoDataType)
  alignment _ = (#alignment TangoDataType)
  peek = peekBounded "data type"
  poke = pokeBounded "data type"

stringToVector :: String -> V.Vector CChar
stringToVector s = V.fromList (castCharToCChar <$> s)

data HaskellDataFormat = HaskellScalar
                       | HaskellSpectrum
                       | HaskellImage
                       deriving(Show)

instance Storable HaskellDataFormat where
  sizeOf _ = (#size AttrDataFormat)
  alignment _ = (#alignment AttrDataFormat)
  peek ptr = do
    value :: CInt <- peek (castPtr ptr)
    case value of
      0 -> pure HaskellScalar
      1 -> pure HaskellSpectrum
      _ -> pure HaskellImage
  poke ptr x = poke @CInt (castPtr ptr) (case x of
                                           HaskellScalar -> 0
                                           HaskellSpectrum -> 1
                                           HaskellImage -> 2)
  

data HaskellDataQuality = HaskellValid
                        | HaskellInvalid
                        | HaskellAlarm
                        | HaskellChanging
                        | HaskellWarning
                        deriving(Show)



data Timeval = Timeval {
  -- Guesswork, not sure how to type it
    tvSec :: !CLong
  , tvUsec :: !CLong
  } deriving(Show)

instance Storable Timeval where
  sizeOf _ = (#size timeval)
  alignment _ = (#alignment timeval)
  peek ptr = do
    tvSec' <- (#peek timeval, tv_sec) ptr
    tvUsec' <- (#peek timeval, tv_usec) ptr
    pure (Timeval tvSec' tvUsec')
  poke ptr (Timeval tvSec' tvUsec') = do
    (#poke timeval, tv_sec) ptr tvSec'
    (#poke timeval, tv_usec) ptr tvUsec'

data HaskellAttrWriteType = Read | ReadWithWrite | Write | ReadWrite deriving(Show)

instance Storable HaskellAttrWriteType where
  sizeOf _ = (#size AttrWriteType)
  alignment _ = (#alignment AttrWriteType)
  peek ptr = do
    value :: CInt <- peek (castPtr ptr)
    case value of
      0 -> pure Read
      1 -> pure ReadWithWrite
      2 -> pure Write
      _ -> pure ReadWrite
  poke ptr x = poke @CInt (castPtr ptr) (case x of
                                           Read -> 0
                                           ReadWithWrite -> 1
                                           Write -> 2
                                           ReadWrite -> 3)
  
  
data HaskellDispLevel = Operator | Expert deriving(Show, Eq)

instance Storable HaskellDispLevel where
  sizeOf _ = (#size DispLevel)
  alignment _ = (#alignment DispLevel)
  peek ptr = do
    value :: CInt <- peek (castPtr ptr)
    case value of
      0 -> pure Operator
      _ -> pure Expert
  poke ptr x = poke @CInt (castPtr ptr) (if x == Operator then 0 else 1)

type VectorCString = V.Vector CChar
  
data HaskellAttributeInfo = HaskellAttributeInfo
  { attributeInfoName :: !VectorCString
  , attributeInfoWritable :: !HaskellAttrWriteType
  , attributeInfoDataFormat :: !HaskellDataFormat
  , attributeInfoDataType :: !HaskellTangoDataType
  , attributeInfoMaxDimX :: !Int
  , attributeInfoMaxDimY :: !Int
  , attributeInfoDescription :: !VectorCString
  , attributeInfoLabel :: !VectorCString
  , attributeInfoUnit :: !VectorCString
  , attributeInfoStandardUnit :: !VectorCString
  , attributeInfoDisplayUnit :: !VectorCString
  , attributeInfoFormat :: !VectorCString
  , attributeInfoMinValue :: !VectorCString
  , attributeInfoMaxValue :: !VectorCString
  , attributeInfoMinAlarm :: !VectorCString
  , attributeInfoMaxAlarm :: !VectorCString
  , attributeInfoWritableAttrName :: !VectorCString
  , attributeInfoDispLevel :: !HaskellDispLevel
  } deriving(Show)

instance Storable HaskellAttributeInfo where
  sizeOf _ = (#{size AttributeInfo})
  alignment _ = (#alignment AttributeInfo)
  peek ptr = do
    name' <- cStringToVector <$> ((#peek AttributeInfo, name) ptr)
    description' <- cStringToVector <$> ((#peek AttributeInfo, description) ptr)
    label' <- cStringToVector <$> ((#peek AttributeInfo, label) ptr)
    unit' <- cStringToVector <$> ((#peek AttributeInfo, unit) ptr)
    standard_unit' <- cStringToVector <$> ((#peek AttributeInfo, standard_unit) ptr)
    display_unit' <- cStringToVector <$> ((#peek AttributeInfo, display_unit) ptr)
    format' <- cStringToVector <$> ((#peek AttributeInfo, format) ptr)
    min_value' <- cStringToVector <$> ((#peek AttributeInfo, min_value) ptr)
    max_value' <- cStringToVector <$> ((#peek AttributeInfo, max_value) ptr)
    min_alarm' <- cStringToVector <$> ((#peek AttributeInfo, min_alarm) ptr)
    max_alarm' <- cStringToVector <$> ((#peek AttributeInfo, max_alarm) ptr)
    writable_attr_name' <- cStringToVector <$> ((#peek AttributeInfo, writable_attr_name) ptr)
    HaskellAttributeInfo
         <$> name'
         <*> ((#peek AttributeInfo, writable) ptr)
         <*> ((#peek AttributeInfo, data_format) ptr)
         <*> ((#peek AttributeInfo, data_type) ptr)
         <*> ((#peek AttributeInfo, max_dim_x) ptr)
         <*> ((#peek AttributeInfo, max_dim_y) ptr)
         <*> description'
         <*> label'
         <*> unit'
         <*> standard_unit'
         <*> display_unit'
         <*> format'
         <*> min_value'
         <*> max_value'
         <*> min_alarm'
         <*> max_alarm'
         <*> writable_attr_name'
         <*> ((#peek AttributeInfo, disp_level) ptr)
  poke ptr (HaskellAttributeInfo name' writable' dataFormat' dataType' maxDimX' maxDimY' description' label' unit' standardUnit' displayUnit' format' minValue' maxValue' minAlarm' maxAlarm' writableAttrName' dispLevel') = do
    V.unsafeWith name' ((#poke AttributeInfo, name) ptr)
    (#poke AttributeInfo, writable) ptr writable'
    (#poke AttributeInfo, data_format) ptr dataFormat'
    (#poke AttributeInfo, data_type) ptr dataType'
    (#poke AttributeInfo, max_dim_x) ptr maxDimX'
    (#poke AttributeInfo, max_dim_y) ptr maxDimY'
    V.unsafeWith description' ((#poke AttributeInfo, description) ptr)
    V.unsafeWith label' ((#poke AttributeInfo, label) ptr)
    V.unsafeWith unit' ((#poke AttributeInfo, unit) ptr)
    V.unsafeWith standardUnit' ((#poke AttributeInfo, standard_unit) ptr)
    V.unsafeWith displayUnit' ((#poke AttributeInfo, display_unit) ptr)
    V.unsafeWith format' ((#poke AttributeInfo, format) ptr)
    V.unsafeWith minValue' ((#poke AttributeInfo, min_value) ptr)
    V.unsafeWith maxValue' ((#poke AttributeInfo, max_value) ptr)
    V.unsafeWith minAlarm' ((#poke AttributeInfo, min_alarm) ptr)
    V.unsafeWith maxAlarm' ((#poke AttributeInfo, max_alarm) ptr)
    V.unsafeWith writableAttrName' ((#poke AttributeInfo, writable_attr_name) ptr)
    (#poke AttributeInfo, disp_level) ptr dispLevel'
  
data HaskellDbDatum = HaskellDbDatum
  { dbDatumPropertyName :: !VectorCString
  , dbDatumIsEmpty :: !Bool
  , dbDatumWrongDataType :: !Bool
  , dbDatumDataType :: !HaskellTangoDataType
  , dbDatumPropData :: !HaskellTangoPropertyData
  } deriving(Show)

data HaskellAttributeData = HaskellAttributeData
  { dataFormat :: !HaskellDataFormat
  , dataQuality :: !HaskellDataQuality
  , nbRead :: !CLong
  , name :: !VectorCString
  , dimX :: !Int32
  , dimY :: !Int32
  , timeStamp :: !Timeval
  , dataType :: !HaskellTangoDataType
  , tangoAttributeData :: !HaskellTangoAttributeData
  } deriving(Show)

data HaskellCommandData = HaskellCommandData
  { argType :: !HaskellTangoDataType
  , tangoCommandData :: !HaskellTangoCommandData
  } deriving(Show)

haskellDisplayLevelOperator :: CInt
haskellDisplayLevelOperator = 0

haskellDisplayLevelExpert :: CInt
haskellDisplayLevelExpert = 1

data HaskellCommandInfo = HaskellCommandInfo
  { cmdName :: !VectorCString
  , cmdTag :: !Int32
  , cmdInType :: !Int32
  , cmdOutType :: !Int32
  , cmdInTypeDesc :: !VectorCString
  , cmdOutTypeDesc :: !VectorCString
  , cmdDisplayLevel :: !CInt
  } deriving(Show)

cStringToVector :: CString -> IO VectorCString
cStringToVector cstr = do
  asString <- peekCString cstr
  pure (V.fromList (castCharToCChar <$> asString))

instance Storable HaskellCommandInfo where
  sizeOf _ = (#{size CommandInfo})
  alignment _ = (#alignment CommandInfo)
  peek ptr = do
    cmd_name' <- (#peek CommandInfo, cmd_name) ptr
    cmdNameAsVector <- cStringToVector cmd_name'
    cmd_tag' <- (#peek CommandInfo, cmd_tag) ptr
    in_type' <- (#peek CommandInfo, in_type) ptr
    out_type' <- (#peek CommandInfo, out_type) ptr
    in_type_desc' <- (#peek CommandInfo, in_type_desc) ptr
    inTypeDescAsVector <- cStringToVector in_type_desc'
    out_type_desc' <- (#peek CommandInfo, out_type_desc) ptr
    outTypeDescAsVector <- cStringToVector out_type_desc'
    disp_level' <- (#peek CommandInfo, disp_level) ptr
    pure (HaskellCommandInfo cmdNameAsVector cmd_tag' in_type' out_type' inTypeDescAsVector outTypeDescAsVector disp_level')
  -- I see no reason why we'd ever poke this (i.e. write an info struct)
  poke ptr (HaskellCommandInfo cmd_name' cmd_tag' in_type' out_type' in_type_desc' out_type_desc' disp_level') = do
    V.unsafeWith cmd_name' ((#poke CommandInfo, cmd_name) ptr)
    (#poke CommandInfo, cmd_tag) ptr cmd_tag'
    (#poke CommandInfo, in_type) ptr in_type'
    (#poke CommandInfo, out_type) ptr out_type'
    V.unsafeWith in_type_desc' ((#poke CommandInfo, in_type_desc) ptr)
    V.unsafeWith out_type_desc' ((#poke CommandInfo, out_type_desc) ptr)
    (#poke CommandInfo, disp_level) ptr disp_level'
    

qualityToHaskell :: CInt -> HaskellDataQuality
qualityToHaskell 0 = HaskellValid
qualityToHaskell 1 = HaskellInvalid
qualityToHaskell 2 = HaskellAlarm
qualityToHaskell 3 = HaskellChanging
qualityToHaskell _ = HaskellWarning

instance Storable HaskellDbDatum where
  sizeOf _ = (#{size DbDatum})
  alignment _ = (#alignment DbDatum)
  peek ptr = do
    property_name' <- (#peek DbDatum, property_name) ptr
    propertyNameAsVector <- cStringToVector property_name'
    data_type' <- (#peek DbDatum, data_type) ptr
    is_empty' <- (#peek DbDatum, is_empty) ptr
    wrong_data_type' <- (#peek DbDatum, wrong_data_type) ptr
    let withoutType = HaskellDbDatum
                      propertyNameAsVector
                      is_empty'
                      wrong_data_type'
    case data_type' of
      HaskellDevVarBooleanArray -> do
        prop_data' :: HaskellVarBoolArray <- (#peek DbDatum, prop_data) ptr
        pure (withoutType HaskellDevBoolean (HaskellPropBoolArray prop_data'))
      HaskellDevVarStringArray -> do
        prop_data' :: HaskellVarStringArray <- (#peek DbDatum, prop_data) ptr
        pure (withoutType HaskellDevString (HaskellPropStringArray prop_data'))
      _ -> error ("not supported data type: " <> show data_type')
  poke ptr haskellDbDatum = do
    V.unsafeWith (dbDatumPropertyName haskellDbDatum) $ \namePtr -> (#poke DbDatum, property_name) ptr namePtr
    (#poke DbDatum, is_empty) ptr (dbDatumIsEmpty haskellDbDatum)
    (#poke DbDatum, wrong_data_type) ptr (dbDatumWrongDataType haskellDbDatum)
    (#poke DbDatum, data_type) ptr (dbDatumDataType haskellDbDatum)
    case dbDatumPropData haskellDbDatum of
      HaskellPropDoubleArray doubles' -> do
        (#poke DbDatum, prop_data) ptr doubles'
      HaskellPropStringArray strings' -> do
        (#poke DbDatum, prop_data) ptr strings'
      _ -> pure ()


instance Storable HaskellAttributeData where
  sizeOf _ = (#{size AttributeData})
  alignment _ = (#alignment AttributeData)
  peek ptr = do
    data_type' <- (#peek AttributeData, data_type) ptr
    dim_x' <- (#peek AttributeData, dim_x) ptr
    dim_y' <- (#peek AttributeData, dim_y) ptr
    name' <- (#peek AttributeData, name) ptr
    nameAsVector <- cStringToVector name'
    nb_read' <- (#peek AttributeData, nb_read) ptr
    quality' <- (#peek AttributeData, quality) ptr
    data_format' <- (#peek AttributeData, data_format) ptr
    time_stamp' <- (#peek AttributeData, time_stamp) ptr
    let withoutType = HaskellAttributeData
                      data_format'
                      (qualityToHaskell quality')
                      nb_read'
                      nameAsVector
                      dim_x'
                      dim_y'
                      time_stamp'
    case data_type' of
      HaskellDevDouble -> do
        attr_data' :: HaskellVarDoubleArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevDouble (HaskellDoubleArray attr_data'))
      HaskellDevVarBooleanArray -> do
        attr_data' :: HaskellVarBoolArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevBoolean (HaskellBoolArray attr_data'))
      HaskellDevVarStringArray -> do
        attr_data' :: HaskellVarStringArray <- (#peek AttributeData, attr_data) ptr
        pure (withoutType HaskellDevString (HaskellStringArray attr_data'))
      _ -> error ("not supported data type: " <> show data_type')
  poke ptr haskellAttributeData = do
    (#poke AttributeData, dim_x) ptr (dimX haskellAttributeData)
    (#poke AttributeData, dim_y) ptr (dimY haskellAttributeData)
    V.unsafeWith (name haskellAttributeData) $ \namePtr -> (#poke AttributeData, name) ptr namePtr
    (#poke AttributeData, data_type) ptr (dataType haskellAttributeData)
    case tangoAttributeData haskellAttributeData of
      HaskellDoubleArray doubles' -> do
        (#poke AttributeData, attr_data) ptr doubles'
      HaskellStringArray strings' -> do
        (#poke AttributeData, attr_data) ptr strings'
      _ -> pure ()
      
instance Storable HaskellCommandData where
  sizeOf _ = (#{size CommandData})
  alignment _ = (#alignment CommandData)
  peek ptr = do
    data_type' <- (#peek CommandData, arg_type) ptr
    case data_type' :: CInt of
      5 -> do
        cmd_data' :: CDouble <- (#peek CommandData, cmd_data) ptr
        pure (HaskellCommandData HaskellDevDouble (HaskellCommandDouble cmd_data'))
      8 -> do
        cmd_data' :: CString <- (#peek CommandData, cmd_data) ptr
        real_string <- peekCString cmd_data'
        pure (HaskellCommandData HaskellDevString (HaskellCommandString (V.fromList (castCharToCChar <$> real_string))))
      _ -> error "shit"
  poke ptr haskellCommandData = do
    (#poke CommandData, arg_type) ptr (argType haskellCommandData)
    case tangoCommandData haskellCommandData of
      HaskellCommandDouble double -> do
        hPutStrLn stderr "poking double"
        (#poke CommandData, cmd_data) ptr double
      HaskellCommandString charVector -> do
        hPutStrLn stderr "poking string"
        V.unsafeWith charVector ((#poke CommandData, cmd_data) ptr)

data HaskellDevFailed a = HaskellDevFailed
  { devFailedDesc :: !a,
    devFailedReason :: !a,
    devFailedOrigin :: !a,
    devFailedSeverity :: !CInt
  } deriving(Functor, Foldable, Traversable)

data HaskellErrorStack = HaskellErrorStack
  { errorStackLength :: !Word32,
    errorStackSequence :: !(Ptr (HaskellDevFailed CString))
  }

instance Storable HaskellErrorStack where
  sizeOf _ = (#size ErrorStack)
  alignment _ = (#alignment ErrorStack)
  peek ptr = do
    length' <- (#peek ErrorStack, length) ptr
    sequence' <- (#peek ErrorStack, sequence) ptr
    pure (HaskellErrorStack length' sequence')
  poke _ _ = error "HaskellErrorStack not pokeable"
  

instance Storable a => Storable (HaskellDevFailed a) where
  sizeOf _ = (#size ErrorStack)
  alignment _ = (#alignment ErrorStack)
  peek ptr = do
    desc' <- (#peek DevFailed, desc) ptr
    reason' <- (#peek DevFailed, reason) ptr
    origin' <- (#peek DevFailed, origin) ptr
    severity' <- (#peek DevFailed, severity) ptr
    pure (HaskellDevFailed desc' reason' origin' severity')
  poke ptr (HaskellDevFailed desc' reason' origin' _severity) = do
    (#poke DevFailed, desc) ptr desc'
    (#poke DevFailed, reason) ptr reason'
    (#poke DevFailed, origin) ptr origin'

newtype HaskellVarDoubleArray = HaskellVarDoubleArray {
    doubles :: V.Vector CDouble
  } deriving(Show)
  
newtype HaskellVarBoolArray = HaskellVarBoolArray {
    bools :: V.Vector CBool
  } deriving(Show)

newtype HaskellCommandInfoList = HaskellCommandInfoList {
    commandInfos :: V.Vector HaskellCommandInfo
  } deriving(Show)

instance Storable HaskellCommandInfoList where
  sizeOf _ = (#size CommandInfoList)
  alignment _ = (#alignment CommandInfoList)
  peek ptr = do
    length' :: CULong <- (#peek CommandInfoList, length) ptr
    sequence' <- (#peek CommandInfoList, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length') sequence'
    let vector = V.fromList haskellSequence
    pure (HaskellCommandInfoList vector)
  poke ptr (HaskellCommandInfoList content) = do
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke CommandInfoList, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke CommandInfoList, sequence) ptr vptr
    
newtype HaskellAttributeInfoList = HaskellAttributeInfoList {
    attributeInfos :: V.Vector HaskellAttributeInfo
  } deriving(Show)

instance Storable HaskellAttributeInfoList where
  sizeOf _ = (#size AttributeInfoList)
  alignment _ = (#alignment AttributeInfoList)
  peek ptr = do
    length' :: CULong <- (#peek AttributeInfoList, length) ptr
    sequence' <- (#peek AttributeInfoList, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length') sequence'
    let vector = V.fromList haskellSequence
    pure (HaskellAttributeInfoList vector)
  poke ptr (HaskellAttributeInfoList content) = do
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke AttributeInfoList, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke AttributeInfoList, sequence) ptr vptr

newtype HaskellAttributeDataList = HaskellAttributeDataList {
    attributeDatas :: V.Vector HaskellAttributeData
  } deriving(Show)

instance Storable HaskellAttributeDataList where
  sizeOf _ = (#size AttributeDataList)
  alignment _ = (#alignment AttributeDataList)
  peek ptr = do
    length' :: CULong <- (#peek AttributeDataList, length) ptr
    sequence' <- (#peek AttributeDataList, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length') sequence'
    let vector = V.fromList haskellSequence
    pure (HaskellAttributeDataList vector)
  poke ptr (HaskellAttributeDataList content) = do
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke AttributeDataList, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke AttributeDataList, sequence) ptr vptr


instance Storable HaskellVarDoubleArray where
  sizeOf _ = (#size VarDoubleArray)
  alignment _ = (#alignment VarDoubleArray)
  peek ptr = do
    length' :: CULong <- (#peek VarDoubleArray, length) ptr
    sequence' <- (#peek VarDoubleArray, sequence) ptr
    haskellSequence <- peekArray (fromIntegral length') sequence'
    pure (HaskellVarDoubleArray (V.fromList haskellSequence))
  poke ptr (HaskellVarDoubleArray content) = do
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarDoubleArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarDoubleArray, sequence) ptr vptr
    

instance Storable HaskellVarBoolArray where
  sizeOf _ = (#size VarBoolArray)
  alignment _ = (#alignment VarBoolArray)
  peek ptr = do
    length' :: CULong <- (#peek VarBoolArray, length) ptr
    sequence' <- (#peek VarBoolArray, sequence) ptr
    -- FIXME: we could use a custom peekArray for Vectors to be faster possibly?
    haskellSequence <- peekArray (fromIntegral length') sequence'
    pure (HaskellVarBoolArray (V.fromList haskellSequence))
  poke ptr (HaskellVarBoolArray content) = do
    hPutStrLn stderr "poking varboolarray"
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarBoolArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarBoolArray, sequence) ptr vptr

newtype HaskellVarStringArray = HaskellVarStringArray {
    strings :: V.Vector CString
  } deriving(Show)

instance Storable HaskellVarStringArray where
  sizeOf _ = (#size VarStringArray)
  alignment _ = (#alignment VarStringArray)
  peek ptr = do
    length' :: CULong <- (#peek VarStringArray, length) ptr
    sequence' <- (#peek VarStringArray, sequence) ptr
    -- FIXME: we could use a custom peekArray for Vectors to be faster possibly?
    haskellSequence <- peekArray (fromIntegral length') sequence'
    pure (HaskellVarStringArray (V.fromList haskellSequence))
  poke ptr ( HaskellVarStringArray content) = do
    let len :: Word32
        len = fromIntegral (V.length content)
    (#poke VarStringArray, length) ptr len
    V.unsafeWith content $ \vptr -> (#poke VarStringArray, sequence) ptr vptr

type DeviceProxyPtr = Ptr ()
type TangoError = Ptr HaskellErrorStack

foreign import ccall unsafe "c_tango.h tango_create_device_proxy"
     tango_create_device_proxy :: CString -> Ptr DeviceProxyPtr -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_delete_device_proxy"
     tango_delete_device_proxy :: DeviceProxyPtr -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_read_attribute"
     tango_read_attribute :: DeviceProxyPtr -> CString -> Ptr HaskellAttributeData -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_write_attribute"
     tango_write_attribute :: DeviceProxyPtr -> Ptr HaskellAttributeData -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_command_inout"
     tango_command_inout :: DeviceProxyPtr -> CString -> Ptr HaskellCommandData -> Ptr HaskellCommandData -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_free_AttributeData"
     tango_free_AttributeData :: Ptr HaskellAttributeData -> IO ()

foreign import ccall unsafe "c_tango.h tango_free_CommandData"
     tango_free_CommandData :: Ptr HaskellCommandData -> IO ()

foreign import ccall unsafe "c_tango.h tango_free_VarStringArray"
     tango_free_VarStringArray :: Ptr HaskellVarStringArray -> IO ()

foreign import ccall unsafe "c_tango.h tango_set_timeout_millis"
     tango_set_timeout_millis :: DeviceProxyPtr -> CInt -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_timeout_millis"
     tango_get_timeout_millis :: DeviceProxyPtr -> Ptr CInt -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_set_source"
     tango_set_source :: DeviceProxyPtr -> CInt -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_source"
     tango_get_source :: DeviceProxyPtr -> Ptr CInt -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_lock"
     tango_lock :: DeviceProxyPtr -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_unlock"
     tango_unlock :: DeviceProxyPtr -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_is_locked"
     tango_is_locked :: DeviceProxyPtr -> Ptr Bool -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_is_locked_by_me"
     tango_is_locked_by_me :: DeviceProxyPtr -> Ptr Bool -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_locking_status"
     tango_locking_status :: DeviceProxyPtr -> Ptr CString -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_command_list_query"
     tango_command_list_query :: DeviceProxyPtr -> Ptr HaskellCommandInfoList -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_free_CommandInfoList"
     tango_free_CommandInfoList :: Ptr HaskellCommandInfoList -> IO ()

foreign import ccall unsafe "c_tango.h tango_get_attribute_list"
     tango_get_attribute_list :: DeviceProxyPtr -> Ptr HaskellVarStringArray -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_attribute_config"
     tango_get_attribute_config :: DeviceProxyPtr -> Ptr HaskellVarStringArray -> Ptr HaskellAttributeInfoList -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_read_attributes"
     tango_read_attributes :: DeviceProxyPtr -> Ptr HaskellVarStringArray -> Ptr HaskellAttributeDataList -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_create_database_proxy"
     tango_create_database_proxy :: Ptr DeviceProxyPtr -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_delete_database_proxy"
     tango_delete_database_proxy :: DeviceProxyPtr -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_device_exported"
     tango_get_device_exported :: DeviceProxyPtr -> CString -> Ptr HaskellDbDatum -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_device_exported_for_class"
     tango_get_device_exported_for_class :: DeviceProxyPtr -> CString -> Ptr HaskellDbDatum -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_object_list"
     tango_get_object_list :: DeviceProxyPtr -> CString -> Ptr HaskellDbDatum -> IO TangoError

foreign import ccall unsafe "c_tango.h tango_get_object_property_list"
     tango_get_object_property_list :: DeviceProxyPtr -> CString -> CString -> Ptr HaskellDbDatum -> IO TangoError

