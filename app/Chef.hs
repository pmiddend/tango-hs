{-# LANGUAGE OverloadedStrings #-}

module Main where

import AmarcordApi
import AmarcordApi.Common
import qualified Control.Monad.Logger as Log
import Data.Text (Text)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified Tango as Tango
import qualified TangoHL as TangoHL

data CliOptions = CliOptions
  { cliBeamtimeId :: Int,
    cliDataSetId :: Int,
    cliIndexedFpsLowWatermark :: Float,
    cliTargetIndexedFrames :: Int,
    cliFramesPerRun :: Int,
    cliAmarcordUrl :: Text,
    cliP11RunnerIdentifier :: Text
  }

cliOptionsParser :: Opt.Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> Opt.option Opt.auto (Opt.long "beamtime-id")
    <*> Opt.option Opt.auto (Opt.long "data-set-id")
    <*> Opt.option Opt.auto (Opt.long "indexed-fps-low-watermark")
    <*> Opt.option Opt.auto (Opt.long "target-indexed-frames")
    <*> Opt.option Opt.auto (Opt.long "frames-per-run")
    <*> Opt.strOption (Opt.long "amarcord-url")
    <*> Opt.strOption (Opt.long "p11-runner-identifier")

withRunner :: CliOptions -> Tango.DeviceProxyPtr -> IO ()
withRunner cliOptions runner = do
  result <-
    runWithConfiguration
      (defaultConfiguration {configBaseURL = cliAmarcordUrl cliOptions})
      (readAnalysisResultsApiAnalysisAnalysisResults_BeamtimeId_Get (cliBeamtimeId cliOptions))
  print result

main :: IO ()
main = do
  let opts =
        Opt.info
          (cliOptionsParser <**> Opt.helper)
          ( Opt.fullDesc <> Opt.progDesc "Automate CFEL TapeDrive 2.0 data collection" <> Opt.header "chef - cook some crystal recipes!"
          )
  cliOptions <- Opt.execParser opts
  TangoHL.withDeviceProxy (cliP11RunnerIdentifier cliOptions) (withRunner cliOptions)
