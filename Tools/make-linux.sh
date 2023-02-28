#!/bin/bash

if [ "$1" == "" ]
then
echo "First argument is the SIMPLECRYPTKEY to use! Maybe try -DOLLAR_SIGN-SIMPLECRYPTKEY?"
#exit 1
echo "For now it will simply be ignored and default is used"
fi

CRYPTKEY=$1

. qt_dir_finder.sh

STARTDIR=`pwd`
echo "This script is running under the assumption it is based in the Tools folder of your local jasp-desktop clone, if this is not the case it is likely to complain.."
cd ..
if [ ! -d "Tools" ]
then
  echo "Are you running this script from Tools ?"
  exit 1
fi
cd ..

JASP_ROOT_DIR=$STARTDIR/../..
#JASP_REQUIRED_FILES_DIR=jasp-required-files
JASP_BUILD_DIR=jasp-build
JASP_GIT_DIR=build #for buildbot

JASP_FULL_BUILD_DIR=$JASP_ROOT_DIR/$JASP_BUILD_DIR
JASP_FULL_GIT_DIR=$JASP_ROOT_DIR/$JASP_GIT_DIR

if [ ! -d $JASP_GIT_DIR ]
then
  echo "Missing folder with git clone of repository: $JASP_GIT_DIR\nExiting script"
  exit 1
fi

#if [ ! -d $JASP_REQUIRED_FILES_DIR ]
#then
#  echo "Missing folder with required binaries etc: $JASP_REQUIRED_FILES_DIR\nExiting script"
#  exit 1
#fi

echo "Cleaning up previous build folder ($JASP_BUILD_DIR) and creating it if necessary"
rm -rf $JASP_BUILD_DIR
mkdir $JASP_BUILD_DIR

#echo "Copying files from $JASP_REQUIRED_FILES_DIR to $JASP_BUILD_DIR"
#cp $JASP_REQUIRED_FILES_DIR/* $JASP_BUILD_DIR/

#echo "Get the latest version of development from github!"
cd $JASP_GIT_DIR
#git fetch origin
#git checkout development
#git pull

echo "Running qmake!"
if [ "$CRYPTKEY" != "" ]
then
    $QT_KIT_FULL/bin/qmake -set ENVIRONMENT_CRYPTKEY \$CRYPTKEY\ || exit 1
fi
$QT_KIT_FULL/bin/qmake -makefile -nocache -o ../$JASP_BUILD_DIR/Makefile JASP.pro || exit 1
$QT_KIT_FULL/bin/qmake -set ENVIRONMENT_CRYPTKEY \"\" || exit 1

echo "Compiling JASP!"
cd ../$JASP_BUILD_DIR
make -j`nproc --all` || exit 1

echo "Compiling finished succesfully!"

