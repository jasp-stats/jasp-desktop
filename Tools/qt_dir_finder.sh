#!/bin/sh
QT_KIT="$QT_KIT"
QT_DIR="$QT_DIR"
QT_VERSION="$QT_VERSION"

if [ -z "$QT_KIT" ]
then
UNAME="$(uname -s)"
if [ "$UNAME" = "Darwin" ]
then
QT_KIT=clang_64
else
QT_KIT=gcc_64
fi
fi

if [ -z "$QT_DIR" ]
then
QT_DIR=~/Qt
fi

if [ -z "$QT_VERSION" ]
then
QT_VERSION=5.15.2
fi

echo "Using QT_DIR $QT_DIR, QT_VERSION $QT_VERSION and QT_KIT $QT_KIT"

QT_KIT_FULL="$QT_DIR/$QT_VERSION/$QT_KIT"

if [ ! -d "$QT_KIT_FULL" ]
then
    echo "Cannot find QT_KIT folder! $QT_KIT_FULL"
    exit 1
fi
