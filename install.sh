#! /bin/bash

source config.rc

mkdir -p $HOME/.racket/5.1.3/collects/relation-map
cp *.rkt $HOME/.racket/5.1.3/collects/relation-map/.

mkdir -p $HOME/.config/relation-map
cp config.rc $HOME/.config/relation-map/.

cp relation-map $RELATION_MAP_BIN/.
cp write-dot-file $RELATION_MAP_BIN/write-relation-map
