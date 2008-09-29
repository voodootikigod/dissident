#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/mochiweb/ebin $PWD/webmachine/ebin -boot start_sasl -s reloader -s webmachine_demo
