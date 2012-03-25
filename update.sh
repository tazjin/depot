#!/bin/bash

hg pull
hg update
cabal install --reinstall