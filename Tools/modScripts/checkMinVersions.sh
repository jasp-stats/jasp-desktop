#!/bin/bash

# while working on making sure notarizing works on macOS I had to fix quite some things, see issue https://github.com/jasp-stats/jasp-test-release/issues/507 (although that might not be linked to all changes)
# after getting the whole up and running I ran into some problems with hardened runtimes. This script is a recursive version of a small snippet of code in this forum post dealing with the same problem: https://forums.developer.apple.com/thread/115451
# if run on jasp-required-files it will list all the libraries that miss this information. After running create-r-framework script the only one shown should be libreadline.5.2.dylib, well, I wish it wasn't but probably will though...

listVersionMin () {
	#make sure all executable files, or those with dylib or so as extension are signed
	for f in "$1"/*; do
		if [[  -f "$f" && ( $f =~ [[:\<:]].+\.(dylib|so)[[:\>:]] ) ]]; then
			
			GREP_FOUND_THIS=`otool -l "$f" | grep -B 1 -A 3 LC_VERSION_MIN`
			if [[ "$GREP_FOUND_THIS" == "" ]]; then
				echo Library misses minimal version info, should be recompiled:    $f 
			fi
		fi
	done

	#Descend depth-first the go deeper into each folder here
	for d in "$1"/*/; do
		if [ -d "$d" ]; then
			listVersionMin "$d"
		fi
	done
}

listVersionMin "$1"
