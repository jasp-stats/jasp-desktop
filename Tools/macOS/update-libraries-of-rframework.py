#!/usr/bin/python

import os
import sys
from subprocess import check_output
from subprocess import call
import shutil

current = "4.1-arm64"
if len(sys.argv) > 1 :
	current = sys.argv[1]

linkDir = "@executable_path/.."
if len(sys.argv) > 2 :
	linkDir = "@executable_path/" + sys.argv[2]

print "Current R Version: " + current + ", Replace links of libs with: " + linkDir

def locate_libs(path):


	locations = set()

	if os.path.isfile(path):

		if path.endswith(".so") or path.endswith(".dylib"):

			locations.add(path)
			#print("location: "  + path)

			output = check_output(["otool", "-L", path])

			lines = output.split("\n")
			lines.pop(0)

			for line in lines:

				line = line.strip()
				#print("otool line: " + line)

				if line.startswith("/Library/Frameworks/R.framework/Versions/" + current + "/Resources/") or line.startswith("/opt/") or line.startswith("/usr/local/"):

					file = line.split()[0]
					if os.path.isfile(file):
						locations.add(file)

					if (file != path):
						#print("recursing into " + file)
						locations.update(locate_libs(file))

	elif os.path.isdir(path):


		if path.endswith(".dSYM"):
			shutil.rmtree(path)
			print("removing : " + path)

		elif path.endswith("/html") or path.endswith("/help") or path.endswith("/demo") or path.endswith("/tests"):
			shutil.rmtree(path)
			print("removing : " + path)

		else:

			for sub in os.listdir(path):

				sub_path = os.path.join(path, sub)
				locations.update(locate_libs(sub_path))

	asArray = [ ]
	asArray.extend(locations)

	return asArray

def extract_lib_dependencies(libs):

	dependencies = set()

	for lib in libs:

		output = check_output(["otool", "-L", lib])

		lines = output.split("\n")
		lines.pop(0)

		for line in lines:

			line = line.strip()
			if line.startswith("/Library/Frameworks/R.framework/") or line.startswith("/opt/") or line.startswith("/usr/local/"):

				file = line.split()[0]
				dependencies.add(file)

	asArray = []
	asArray.extend(dependencies)

	return asArray

def change_dep_paths(lib, changes):

	for change in changes:
		print "change link from " + change["old"] + " to " + change["new"] + " of lib " + lib
		call(["install_name_tool", "-change", change["old"], change["new"], lib])


wd = os.getcwd()

#shutil.rmtree(os.path.join(wd, "R.framework"))
#shutil.copytree("/Library/Frameworks/R.framework/", os.path.join(wd, "R.framework"), symlinks=True)

path = os.path.join(wd, "R.framework")
librarypath = os.path.join(wd, "R.framework/Versions/" + current + "/Resources/library")
out_lib_dir = os.path.join(wd, "R.framework/Versions/" + current + "/Resources/lib")

libs = locate_libs(librarypath)
#print(libs)
dependencies = extract_lib_dependencies(libs)
#print(dependencies)

new_libs = [ ]
changes = [ ]

for dependency in dependencies:

	dep_base   = os.path.basename(dependency)
	dep_target = os.path.join(out_lib_dir, dep_base)

	if os.path.isfile(dependency):
		print(dependency + " found!")
#		if dependency != "/opt/X11/lib/libfreetype.6.dylib":
		shutil.copyfile(dependency, dep_target)

		new_libs.append(dep_target)

		change = { "old" : dependency, "new" : linkDir + "/Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + dep_base }
		changes.append(change)

		change = { "old" : dep_base, "new" : linkDir + "/Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + dep_base }
		changes.append(change)

	else:
		print(dependency + " not found!")


#print(dependencies)
for new_lib in new_libs:

	lib_base = os.path.basename(new_lib)
	new_path = linkDir + "/Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + lib_base
	print "change link to " + new_path + " of new lib " + new_lib
	call(["install_name_tool", "-id", new_path, new_lib])

	change_dep_paths(new_lib, changes)

for lib in libs:

	lib_base = os.path.basename(lib)
	new_path = os.path.relpath(lib, path)

	new_path = new_path.replace("R.framework/Resources/", "R.framework/Versions/" + current + "/Resources/")
	new_path = new_path.replace("R.framework/Versions/Current/", "R.framework/Versions/" + current + "/")
	new_path = new_path.replace("R.framework/Libraries/", "R.framework/Versions/" + current + "/lib/")

	if new_path.startswith(".."):
		new_path = linkDir + "/Frameworks/R.framework/Versions/" + current + "/Resources/lib/" + lib_base
	else:

		if new_path.startswith("Resources/"):
			new_path = new_path.replace("Resources/", "Versions/" + current + "/Resources/")
		if new_path.startswith("Versions/Current/"):
			new_path = new_path.replace("Versions/Current/", "Versions/" + current + "/")
		if new_path.startswith("Libraries/"):
			new_path = new_path.replace("Libraries/", "Versions/" + current + "/Resources/")

		new_path = linkDir + "/Frameworks/R.framework/" + new_path

	#print(new_path)

	#print "change link to " + new_path + " of " + lib
	call(["install_name_tool", "-id", new_path, lib])

	change_dep_paths(lib, changes)

def removeFunc(path):
	try:
		os.remove(path)
	except:
		return #we do not care

def removeTreeFunc(path):
	try:
		shutil.rmtree(path)
	except:
		return #we do not care

removeFunc(os.path.join(wd, "R.framework/Headers"))
removeFunc(os.path.join(wd, "R.framework/Libraries"))
removeFunc(os.path.join(wd, "R.framework/PrivateHeaders"))
removeFunc(os.path.join(wd, "R.framework/R"))
removeFunc(os.path.join(wd, "R.framework/Resources"))
removeFunc(os.path.join(wd, "R.framework/Versions/Current"))
removeFunc(os.path.join(wd, "R.framework/Versions/" + current + "/Headers"))
removeFunc(os.path.join(wd, "R.framework/Versions/" + current + "/R"))
removeFunc(os.path.join(wd, "R.framework/Versions/" + current + "/Resources/R"))
removeTreeFunc(os.path.join(wd, "R.framework/Versions/" + current + "/PrivateHeaders"))
removeTreeFunc(os.path.join(wd, "R.framework/Versions/" + current + "/Resources/man1"))
#removeTreeFunc(os.path.join(wd, "R.framework/Versions/" + current + "/Resources/doc"))
