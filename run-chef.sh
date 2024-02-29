#!/usr/bin/env bash

cabal run -- chef --beamtime-id 119 --data-set-id 4233 --indexed-fps-low-watermark 1 --target-indexed-frames 10000 --frames-per-run 100 --amarcord-url http://cfeld-vm04:6020 --p11-runner-identifier 'tango://cfeld-tango:10000/dev_tapedrive/p11/runner'
