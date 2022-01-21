#!/bin/bash

# install_name_prefix_tool
#
# Copyright (c) 2013 Martin Szulecki <martin.szulecki@gmail.com>
#
# Licensed under the GNU General Public License Version 2
#
# This script is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This script is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more profile.
#
# You should have received a copy of the GNU General Public License
# along with this script; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 
# USA

if [ $# -lt 3 ] 
then 
	echo "Usage: ${0##*/} <DIR|FILE> <OLD> <NEW>"
	echo "Changes the library prefix from OLD to NEW for each <DIR>/*.dylib or executable <FILE>."
	exit 1
fi

TARGETS=$1
PREFIX=$2
NEWPREFIX=$3

# echo "TARGETS:   " $TARGETS
# echo "PREFIX:    " $PREFIX
# echo "NEWPREFIX: " $NEWPREFIX

if [[ -d $TARGETS ]]; then
	TARGETS=$TARGETS/*.dylib
elif [[ -f $TARGETS ]]; then
	TARGETS="$TARGETS"
else
    echo "Error: \"$TARGETS\" does not exist." 
    exit 1
fi

INSTALL_NAME_TOOL_BIN=$(which install_name_tool)
OTOOL_BIN=$(which otool)
GREP_BIN=$(which egrep)

for lib in $TARGETS;
do

	lib_basename=$(basename $lib)

	# echo "lib_basename: " $lib_basename

	# echo $($OTOOL_BIN -L $lib)
	# otool -L $lib
	for entry in $($OTOOL_BIN -L $lib | $GREP_BIN -o "$PREFIX/([^[:space:]]*)");
	do
		# echo "entry: " $entry
		entry_basename=$(basename $entry)
		entry_target="$NEWPREFIX/$entry_basename"

		# echo "entry basename: " $entry_basename
		# echo "entry target: " $entry_target

		lib_basename=$(echo $lib_basename | sed 's/.vecLib//g')

		ID_ADD=""
		if [ "$lib_basename" = "$entry_basename" ];
		then
			ID_ADD="-id $entry_target"
			echo "id: " $ID_ADD $lib
			echo
			$INSTALL_NAME_TOOL_BIN $ID_ADD $lib
		fi

		# echo "Changing prefix \"$entry\" to \"$entry_target\"..."
		# echo "$INSTALL_NAME_TOOL_BIN -change $entry $entry_target $lib"
		# echo
		$INSTALL_NAME_TOOL_BIN -change $entry $entry_target $lib
		# 
		# echo "----------"
	done

	# otool -L $lib
	# echo
	# echo "---"
	# echo
done