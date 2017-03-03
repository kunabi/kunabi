#!/bin/sh

# -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:run-lw-android.sh,v 1.4.1.1 2014/05/27 20:55:54 davef Exp $" -*-

# This script runs the LispWorks for Android Runtime image under QEMU.

# These variables can be changed if you have installed the components
# in other directories.
QEMUROOT="$HOME/qemu"
LWANDROIDROOT="$HOME/lw-android"

# Run the Android image using QEMU.
qemubin=qemu-arm
if test `uname` = "Darwin"; then
  export DYLD_LIBRARY_PATH="$QEMUROOT/lib"
  qemubin="$QEMUROOT/bin/$qemubin"
fi
"$qemubin" -L "$QEMUROOT/arm-linux-gnueabihf" "$LWANDROIDROOT/lispworks-7-0-0-arm-linux-android" "$@"
