#!/usr/bin/env bash

# cabal run -- chef --beamtime-id 116 --amarcord-url http://cfeld-vm04:6020 --p11-runner-identifier 'tango://cfeld-tango:10000/dev_tapedrive/p11/runner'

cabal run -- chef --beamtime-id 119 --amarcord-url http://localhost:5001 --p11-runner-identifier 'tango://cfeld-tango:10000/dev_tapedrive/p11/runner'
