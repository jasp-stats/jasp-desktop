#!/usr/bin/python

import os
import sys
import regex
from collections import namedtuple
from enum import Enum

if len(sys.argv) < 4:
	print("Usage: python translationSplitter.py toBeTranslated.po alreadyTranslated.po { po-file-folder | po-file+ }")
	exit(1)

keepTalking = False

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
	MSGCTXT			= 5

msgFactory	= namedtuple('msg', ['msgid', 'msgid_plural', 'msgstrs', 'msgctxt', 'comments'])
msgsToDo	= dict()
msgsDone	= dict()

msgid			= ""
msgid_plural	= ""
msgstrs			= []
msgstate		= parseState.LIMBO
msgctxt			= ""
comments		= []

def resetMsgVars():
	if keepTalking:
		print("Looking for new msg")
	global msgid
	global msgid_plural
	global msgstrs
	global msgstate
	global msgctxt

	msgctxt			= ""
	msgid			= ""
	msgid_plural	= ""
	msgstrs			= []
	msgstate		= parseState.LIMBO

parseMsgstr = regex.compile(r"""\s*
	msgstr(\[\d+\])? 	#msgstr possibly with [#] behind it
	\s+(\"[^\"]*\")			
	""", regex.VERBOSE | regex.MULTILINE)

toDoDone = 0
def storeMsg():
	global msgsDone
	global msgsToDo
	global toDoDone
	global msgid
	global msgid_plural
	global msgstrs
	global msgctxt
	global comments

	translatorFilledAll	= True
	curMsg 				= msgFactory(msgid = msgid, msgid_plural=msgid_plural, msgstrs=msgstrs, msgctxt=msgctxt, comments=comments)

	comments = [] #This is cleared here to make sure we get any comments leading up to the next one

	if keepTalking:
		print("---------------------------------------------------\nStore msg: " + str(curMsg) + "---------------------------------------------------")

	for msgstr in msgstrs:
		m = parseMsgstr.match(msgstr)
		if m:
			cap = m.captures(2)[0]
			if keepTalking:
				print("For msgstr '" + msgstr + "' I find cap: '" + str(cap) + "' and length = " + str(len(cap)))

			if len(cap) == 2: #apparently this one is empty
				translatorFilledAll = False
		else:
			print("Couldnt parse msgstr '" + msgstr + "' for msgid '" + msgid + "' aborting!")
			exit(3)

	if translatorFilledAll:
		if msgid in msgsDone:
			print("msg was filled in twice, msgid doubled:" + msgid + " overwriting it (ctxt:"+msgsDone[msgid].msgctxt+" and msgstrs:" + str(msgsDone[msgid].msgstrs) + ") and keeping the last one (ctxt:" + msgctxt + " and msgstrs: " + str(msgstrs) + ").")
			oldComments = msgsDone[msgid].comments
			for comment in comments:
				oldComments.append(comment)
			comments = oldComments

		if msgid in msgsToDo: #Ok, it is also in the ToDo list, so we can remove it from there now, but keep the comments
			oldComments = msgsToDo[msgid].comments
			for comment in comments:
				oldComments.append(comment)
			comments = oldComments

			#We should keep the context for the one that is not filled in, because it probably is better!
			if msgsToDo[msgid].msgctxt != "":
				if msgctxt == "":
					comments.append("#Context for translated wasn't present")
				else:
					comments.append("#Context for translated: " + msgctxt)
				msgctxt = msgsToDo[msgid].msgctxt
				print("Using context from empty one (" + msgctxt + ")")
			
			del msgsToDo[msgid]
			toDoDone = toDoDone + 1
			
		msgsDone[msgid] = curMsg
	else:
		if keepTalking:
			print("Not filled in...")

		if msgid in msgsDone:
			toDoDone = toDoDone + 1
		else:
			msgsToDo[msgid] = curMsg

def printParseLine(line):
	if keepTalking:
		print("State: " + str(msgstate) + " and line: " + line)

def parseLineLimbo(line):
	global msgid
	global msgstate
	global msgctxt

	printParseLine(line)

	if line == "": #Boring but fine I guess?
		return

	if line.startswith("msgctxt"): #Great!
		resetMsgVars()
		msgctxt 	= line
		msgstate	= parseState.MSGCTXT

	elif line.startswith("msgid"): #Also great!
		resetMsgVars()
		msgid 		= line
		msgstate	= parseState.MSGID
		
	
def parseLineMsgCtxt(line):
	global msgstate
	global msgid
	global msgctxt

	printParseLine(line)

	if line.startswith('"'):
		msgctxt += "\n"
		msgctxt += line
	elif line.startswith("msgid"): #Great!
		msgid 		= line
		msgstate	= parseState.MSGID
	else:
		print("Expected a msgid after msgctxt, but didn't get it!")
		exit(5)
			
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
	
	elif line.startswith("msgid") or line.startswith("msgctxt"):
		msgstate = parseState.LIMBO
		storeMsg()
		parseLineLimbo(line)

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
	parseState.MSGSTRS		: parseLineMsgStrs,
	parseState.MSGCTXT		: parseLineMsgCtxt
}

def parsePoFile(poFilename):
	poFile = open(poFilename)
	poLines = poFile.readlines()

	for poLine in poLines:
		stripped = poLine.strip()
		if  poLine.startswith("#"): 
			comments.append(poLine)
		else:
			parseSwitch[msgstate](poLine.strip())
		

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
		msgctxt			= msg.msgctxt
		comments		= msg.comments

		for comment in comments:
			outFile.write(comment + "\n")

		if msgctxt != "":
			outFile.write(msgctxt + "\n")

		outFile.write(msgid + "\n")

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
print("Of the non-translated msgs there were #" + str(toDoDone) + " that turned out to be translated somewhere already!")