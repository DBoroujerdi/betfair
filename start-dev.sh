#!/bin/sh
cd $(dirname $0)
rm -rf log/*
exec erl \
    +P 5000000 \
    +K true \
    -pa ebin \
    -pa deps/*/ebin \
    -boot start_sasl \
    -config dev.config \
    -s rb \
    -s betfair \
    -sname betfair
