#!/usr/bin/env bash
cd /usr/local/
svn checkout http://svn.plt-scheme.org/plt/trunk plt
cd plt/src
./configure --disable-mred --disable-gl --disable-xrender --disable-xft
make 3m
make install-3m
cd /usr/local/bin
ln -fs /usr/local/plt/bin/mzc3m mzc
ln -fs /usr/local/plt/bin/mzscheme3m mzscheme
ln -fs /usr/local/plt/bin/planet3m planet
ln -fs /usr/local/plt/bin/web-server-text3m web-server-text
ln -fs /usr/local/plt/bin/web-server-setup3m web-server-setup
