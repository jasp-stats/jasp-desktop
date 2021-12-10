#!/usr/bin/python

import os
import sys
from subprocess import check_output
from subprocess import call
import shutil
current = "4.1-arm64"

# Make the changes to the executable path optional
skip_path_changes = False
if len(sys.argv) > 2:
	print("Usage: python " + sys.argv[0] + " [-no-path-change]" )
elif len(sys.argv) > 1:
	if sys.argv[1]=="-no-path-change":
		skip_path_changes = True
	else:
		print("Usage: python " + sys.argv[0] + " [-no-path-change]")
else:
	print("Use the '-no-path-change' option to disable executable path changes" )

print("Skipping executable path changes: ", skip_path_changes)

verbose = True

# run otool -L on libFile and return an array of libs that are loaded by libFile,
# but only if they start with "/Library/Frameworks/R.framework/", "/opt/" or "/usr/local/"
def getOtoolFiles(libFile):
	if not(os.path.isfile(libFile)):
		return set()

	lines = check_output(["otool", "-L", libFile]).split("\n")
	lines.pop(0)
	
	files = set()
	for line in lines:
		line.strip()
		lineParts = line.split()

		if len(lineParts) > 0:
			file = line.split()[0]

			if file.startswith("/Library/Frameworks/R.framework/") or file.startswith("/opt/") or file.startswith("/usr/local/"):
				if os.path.isfile(file):
					files.add(file)
	return files
	

# locate_libs will go through all directories and files in a given directory,
# doing so recursively. It will remove certain folders deemed unnecessary 
# (help, html, demo, tests and .dSym If it finds *.so and *.dylib it will
# use otool to find any other libs needed by the file in question and then
# calls locate_libs on the lines starting with /opt/, /usr/local/ or /Library/Frameworks/R....
def locate_libs(path):
	if verbose:
		print("locate_libs('" + path +"')")
	locations = set()

	if os.path.isdir(path):
		if path.endswith("/html") or path.endswith("/help") or path.endswith("/demo") or path.endswith("/tests") or path.endswith(".dSYM"): 
			shutil.rmtree(path)
			if verbose:
				print("removing : " + path)

		else:
			for sub in os.listdir(path):
				sub_path = os.path.join(path, sub)
				locations.update(locate_libs(sub_path))

	elif os.path.isfile(path):
		if path.endswith(".so") or path.endswith(".dylib"):
			if verbose:
				print('Adding location: ' + path)
			locations.add(path)
			files = getOtoolFiles(path)
	
			for file in files:
				if file.startswith("/Library/Frameworks/R.framework/Versions/" + current + "/Resources/") or file.startswith("/opt/") or file.startswith("/usr/local/"):
					if verbose:
						print('Adding location: ' + file)
					locations.add(file)
					if (file != path):
						locations.update(locate_libs(file))

		if len(locations) > 0 and verbose:
			print('Locations found in lib "' + path + '" are: ' + ", ".join(locations))

	return locations

# Takes all "dependencies" from libs, where dependencies are basically all
# libraries being loaded that are also found by getOtoolFiles
def extract_lib_dependencies(libs):
	dependencies = set()
	for lib in libs:
		dependencies.update(getOtoolFiles(lib))
	return dependencies

wd = os.getcwd()

# Make a fresh version of the framework
# print('Copying the local R.framework to jasp-required-files')
# shutil.rmtree(os.path.join(wd, "R.framework"))
# shutil.copytree("/Library/Frameworks/R.framework/", os.path.join(wd, "R.framework"), symlinks=True)

path			= os.path.join(wd, "R.framework")
libpath 		= os.path.join(wd, "R.framework/Versions/" + current + "/Resources/lib")
out_lib_dir 	= libpath

libs 			= locate_libs(path)
dependencies 	= extract_lib_dependencies(libs)
new_libs 		= [ ]

# We initialize changes with a reference to the bundled JAGS dylib
changes 		= [
	{ "old": "/usr/local/lib/libjags.4.dylib", 	"new": "@executable_path/JAGS/libjags.4.dylib"}
	#, Are no longer necessary with R 4:
	#{ "old": "/usr/lib/libc++abi.dylib",		"new": "@executable_path/../Frameworks/R.framework/Versions/"+current+"/Resources/lib/libc++abi.1.dylib"},
	#{ "old": "/usr/lib/libc++.1.dylib",			"new": "@executable_path/../Frameworks/R.framework/Versions/"+current+"/Resources/lib/libc++.1.dylib"}
	] 

# Adding anything here? Be sure to do the same in jasp-desktop/JASP-Engine/otoolstuff.cpp
# (in branch development and hopefully stable at some point)


if verbose:
	print("libs: " + ",\n\t".join(libs))

# Go through the collected dependencies and make sure they are moved
# to out_lib_dir Also add these new locations to changes where the
# "old" portion is both the full path and the base.
print('Gathering dependencies to be changed')
for dependency in dependencies:
	dep_base   = os.path.basename(dependency)
	dep_target = os.path.join(out_lib_dir, dep_base)

	if os.path.isfile(dependency):
		shutil.copyfile(dependency, dep_target)

		if verbose:
			print('Copying dependency\t'+dependency+'\t->\t'+dep_target)
			
		new_libs.append(dep_target)
		
		newLoc = "@executable_path/../Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + dep_base
		changes.append({ "old" : dependency,	"new" : newLoc })
		changes.append({ "old" : dep_base,		"new" : newLoc })
		
	else:
		print(dependency + " not found!")

if verbose:
	print("dependencies: "+ ", ".join(dependencies))
	print("changes: " + ", ".join(changes))

def installNameToolBase(callList):
	if skip_path_changes:
		if verbose:
			print("skipping: "+ " ".join(callList))
	else:
		if verbose:
			print('calling: ' + ' '.join(callList))
		call(callList)

def installNameToolChange(inLib, replaceThis, withThis):
	installNameToolBase(["install_name_tool", "-change", replaceThis, withThis, inLib])
	
def installNameToolID(inLib, newID):
	installNameToolBase(["install_name_tool", "-id", newID, inLib])

# Run install_name_tool on lib and replaces all lib paths with info in changes = [{old, new}]
def change_dep_paths(lib, changes):
	for change in changes:
		installNameToolChange(lib, change["old"], change["new"])

# run change_dep_paths on all new_libs after setting the proper ID in the lib 
# (because they were copied to another location) with installNameToolID		
for new_lib in new_libs:
	lib_base = os.path.basename(new_lib)
	new_path = "@executable_path/../Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + lib_base
	installNameToolID(new_lib, new_path)
	change_dep_paths(new_lib, changes)

# for use in	the libloop
def littleStartsWithReplacer(new_path, replaceMe, withThis):
	if new_path.startswith(replaceMe):
		new_path = new_path.replace(replaceMe, withThis)
	return new_path

# This is where most of the magic happens, the new_path's for each lib are determined
# and set as ID through installNameToolID and then the paths to their dependencies
# are changed based on what is contained in changes
for lib in libs:
	if lib.startswith(wd): #because otherwise we will try to change stuff in the root directory...
		lib_base = os.path.basename(lib)
		new_path = os.path.relpath(lib, path)
		
		new_path = new_path.replace("R.framework/Resources/",			"R.framework/Versions/" + current + "/Resources/"	)
		new_path = new_path.replace("R.framework/Versions/Current/",	"R.framework/Versions/" + current + "/"				)
		new_path = new_path.replace("R.framework/Libraries/",			"R.framework/Versions/" + current + "/lib/"			)

		if new_path.startswith(".."):
			new_path = "@executable_path/../Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + lib_base
		else:
			new_path = littleStartsWithReplacer(new_path, "Resources/", 			"Versions/" + current + "/Resources/"	)
			new_path = littleStartsWithReplacer(new_path, "Versions/Current/", 		"Versions/" + current + "/"				)
			new_path = littleStartsWithReplacer(new_path, "Libraries/", 			"Versions/" + current + "/Resources/"	)
			new_path = "@executable_path/../Frameworks/R.framework/" + new_path

		if verbose:
			print("lib " + lib + " has new ID: " + new_path)

		installNameToolID(lib, new_path)
		change_dep_paths(lib, changes)

os.remove(		os.path.join(wd, "R.framework/Headers")									)
os.remove(		os.path.join(wd, "R.framework/Libraries")								)
os.remove(		os.path.join(wd, "R.framework/PrivateHeaders")							)
os.remove(		os.path.join(wd, "R.framework/R")										)
os.remove(		os.path.join(wd, "R.framework/Resources")								)
os.remove(		os.path.join(wd, "R.framework/Versions/Current")						)
os.remove(		os.path.join(wd, "R.framework/Versions/" + current + "/Headers")		)
os.remove(		os.path.join(wd, "R.framework/Versions/" + current + "/R")				)
os.remove(		os.path.join(wd, "R.framework/Versions/" + current + "/Resources/R")	)
shutil.rmtree(	os.path.join(wd, "R.framework/Versions/" + current + "/PrivateHeaders")	)
shutil.rmtree(	os.path.join(wd, "R.framework/Versions/" + current + "/Resources/man1")	)
shutil.rmtree(	os.path.join(wd, "R.framework/Versions/" + current + "/Resources/doc")	)

print('Framework is ready for use!')
