#!/bin/sh

set -x
pushd `dirname $0`
aclocal-1.9
autoconf --force
automake-1.9 --add-missing --copy --foreign
popd
