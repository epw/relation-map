#! /usr/bin/bash

source config.rc

mkdir -p $HOME/.config/relation-map
cp config.rc $HOME/.config/relation-map/.

cp relation-map $RELATION_MAP_BIN/.
