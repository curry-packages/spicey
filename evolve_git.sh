#!/bin/bash

# Script to support the evolution of model-based software development
# with Spicey.
# One can use this script if the ER model is changed and one wants
# to generate a new Spicey system based on the new model and include
# all changes made to the old Spicey system.
# These changes are adapted to the new system via the revision control system
# git. Hence, conflicts might occur in the end that must be manually solved!

if [ $# -ne 4 ] ; then
  echo "Illegal number of arguments, usage:"
  echo "$0 ORGPATH MYORGPATH NEWPATH TARGETPATH"
  echo "where"
  echo "ORGPATH   : path to original software (old ER-scheme)"
  echo "MYORGPATH : path to original software (old ER-scheme) with user changes"
  echo "NEWPATH   : path to new software (new ER-scheme)"
  echo "TARGETPATH: path to save new software with user changes"
  exit 1
fi

ORGPATH=$1
MYORGPATH=$2
NEWPATH=$3
TARGETPATH=$4

GIT=`which git`
if [ -z "$GIT" ] ; then
  echo "Revision control system 'git' not found!"
  exit 1
fi

if [ -d "$ORGPATH" ] ; then
  ORGPATH=`cd $ORGPATH && pwd`
else
  echo "Path to original software '$ORGPATH' does not exist!"
  exit 1
fi

if [ -d "$MYORGPATH" ] ; then
  MYORGPATH=`cd $MYORGPATH && pwd`
else
  echo "Original user-changed software path '$MYORGPATH' does not exist!"
  exit 1
fi

if [ -d "$NEWPATH" ] ; then
  NEWPATH=`cd $NEWPATH && pwd`
else
  echo "Path to new software '$NEWPATH' does not exist!"
  exit 1
fi

if [ ! -d "$TARGETPATH" ] ; then
  echo "Target software path '$TARGETPATH' does not exist, trying to create it..."
  mkdir $TARGETPATH
fi

cd $TARGETPATH
$GIT init
cp -r "$ORGPATH"/* .
$GIT add "*"
$GIT commit -m "start"
$GIT branch newscheme
cp -r "$MYORGPATH"/* .
$GIT add *
$GIT commit -m "update to myorg"
$GIT checkout newscheme
cp -r "$NEWPATH"/* .
$GIT add *
$GIT commit -m "update to new scheme"
$GIT checkout master
$GIT merge newscheme
echo "Status after merging:"
$GIT status
