#!/usr/bin/python

import os
import sys
import regex
from collections import namedtuple
from enum import Enum

if len(sys.argv) < 4:
	print("Usage: python translationSplitter.py toBeTranslated.po alreadyTranslated.po { po-file-folder | po-file+ }")
	exit(1)

toBeTranslatedFilename		= sys.argv[1]
alreadyTranslatedFilename	= sys.argv[2]

poFiles = []

#collect the po files we need to check
if len(sys.argv) == 4: #Maybe we've got a po-file-folder here
	possibleFolder = sys.argv[3]
	if os.path.isfile(possibleFolder):
		poFiles.append(possibleFolder)

	elif os.path.isdir(possibleFolder):
		#print("possibleFolder: " + possibleFolder)
		for entry in os.listdir(possibleFolder):
			entryFull = possibleFolder + "/" + entry
			if os.path.isfile(entryFull):
				poFiles.append(entryFull)
else:
	for i in range(3, len(sys.argv)):
		poFiles.append(sys.argv[i])

print(poFiles)

checkUs = poFiles
poFiles = []
for poFile in checkUs:
	if not(poFile.endswith(".po")):
		print("poFile '" + poFile + "' does not end in .po, we will not use it!")
	else:
		poFiles.append(poFile)
		

if len(poFiles) == 0:
	print("You didn't specify any *.po files (or the folder you specified didn't contain any")
	exit(2)

class parseState(Enum):
	LIMBO			= 1
	MSGID			= 2
	MSGID_PLURAL	= 3
	MSGSTRS			= 4

msgFactory	= namedtuple('msg', ['msgid', 'msgid_plural', 'msgstrs'])
msgsToDo	= dict()
msgsDone	= dict()

msgid			= ""
msgid_plural	= ""
msgstrs			= []
msgstate		= parseState.LIMBO

def resetMsgVars():
	global msgid
	global msgid_plural
	global msgstrs
	global msgstate

	msgid			= ""
	msgid_plural	= ""
	msgstrs			= []
	msgstate		= parseState.LIMBO

parseMsgstr = regex.compile(r"""\s*
	msgstr(\[\d+\])? 	#msgstr possibly with [#] behind it
	\s+\"				#we capture till the first quote
	""", regex.VERBOSE | regex.MULTILINE)

def storeMsg():
	global msgsDone
	global msgsToDo
	translatorFilledAll	= True
	curMsg 				= msgFactory(msgid = msgid, msgid_plural=msgid_plural, msgstrs=msgstrs)

	print("---------------------------------------------------\nStore msg: " + str(curMsg))

	for msgstr in msgstrs:
		m = parseMsgstr.match(msgstr)
		if m:
			cap = m.captures(0)[0]

			print("For msgstr '" + msgstr + "' I find cap: '" + str(cap) + "' and diff in lengths = " + str(len(msgstr) - len(cap)))

			if len(msgstr) - len(cap) == 1: #apparently someone filled it in ^^
				translatorFilledAll = False
		else:
			print("Couldnt parse msgstr '" + msgstr + "' for msgid '" + msgid + "' aborting!")
			exit(3)

	if translatorFilledAll:
		if msgid in msgsDone:
			print("msg was filled in twice!\nmsgid doubled:" + msgid + " overwriting it and keeping the last one")
		msgsDone[msgid] = curMsg
	else:
		msgsToDo[msgid] = curMsg

def printParseLine(line):
	print("State: " + str(msgstate) + " and line: " + line)

def parseLineLimbo(line):
	global msgid
	global msgstate
	printParseLine(line)

	if line == "": #Boring but fine I guess?
		return

	if line.startswith("msgid"): #Great!
		resetMsgVars()
		msgid 		= line
		msgstate	= parseState.MSGID
		
	
def parseLineMsgid(line):
	global msgstate
	global msgid
	global msgid_plural
	printParseLine(line)

	if line.startswith('"'):
		msgid += "\n"
		msgid += line

	elif line.startswith("msgid_plural"):
		msgid_plural	= line
		msgstate		= parseState.MSGID_PLURAL

	elif line.startswith("msgstr"):
		msgstrs.append(line)
		msgstate = parseState.MSGSTRS

def parseLineMsgidPlural(line):
	global msgstate
	global msgid_plural
	printParseLine(line)

	if line.startswith('"'):
		msgid_plural += "\n"
		msgid_plural += line

	elif line.startswith("msgstr"):
		msgstrs.append(line)
		msgstate = parseState.MSGSTRS

def parseLineMsgStrs(line):
	global msgstate
	global msgstrs
	printParseLine(line)

	if line.startswith('"'):
		msgstrs[len(msgstrs) - 1] += "\n"
		msgstrs[len(msgstrs) - 1] += line

	elif line.startswith("msgstr"):
		msgstrs.append(line)
	
	elif line.startswith("msgid"):
		msgstate = parseState.LIMBO
		storeMsg()
		parseLineMsgid(line)

	elif line == "": #I guess empty line means we are done with this msg?
		msgstate = parseState.LIMBO
		storeMsg()
	
	else:
		print("I am totally confused with this file, I was expecting something like 'msgstr' but got: " + line)
		exit(4)


parseSwitch = { 
	parseState.LIMBO		: parseLineLimbo,
	parseState.MSGID		: parseLineMsgid,
	parseState.MSGID_PLURAL	: parseLineMsgidPlural,
	parseState.MSGSTRS		: parseLineMsgStrs
}

def parsePoFile(poFilename):
	poFile = open(poFilename)
	poLines = poFile.readlines()

	for poLine in poLines:
		parseSwitch[msgstate](poLine.strip())
		#print("After parseSwitch msgstate: " + str(msgstate))

print("Start parsing")
for poFileName in poFiles:
	parsePoFile(poFileName)
		
print("Im done parsing!")

def writeMsgsToFile(msgs, fileName):
	outFile = open(fileName, "w")
	
	for msgKey in msgs:
		msg 			= msgs[msgKey]
		msgid 			= msg.msgid
		msgid_plural	= msg.msgid_plural
		msgstrs			= msg.msgstrs

		outFile.write(msgid)
		outFile.write("\n")

		if msgid_plural != "":
			outFile.write(msgid_plural)
			outFile.write("\n")

		for msgstr in msgstrs:
			outFile.write(msgstr)
			outFile.write("\n")

		outFile.write("\n")

writeMsgsToFile(msgsDone, alreadyTranslatedFilename)
writeMsgsToFile(msgsToDo, toBeTranslatedFilename)

print("Files written")