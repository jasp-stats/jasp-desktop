#!/usr/bin/python

import sys
import regex

fileIn  = sys.argv[1] if len(sys.argv) > 1 else 'workDir/in'
fileOut = sys.argv[2] if len(sys.argv) > 2 else 'workDir/out'
qprops = open(fileIn)

lines = qprops.readlines()
sizes = [0, 0, 0, 0, 0]

parseLine = regex.compile(r"""\s*
    Q_PROPERTY\(
    (\w+)\s+ #variable type                 #1 
    (\w+)\s+ #prop name                     #2
    READ\s+
    (\w+)\s+ #getter name                   #3
    (WRITE\s+(\w+)\s+)? # optional setter name #4
    NOTIFY\s+
    (\w+) #Notify name                      #5
    \s*\)(.*)
    """, regex.VERBOSE)

def getCapture(m, i):
    capture = m.captures(capF(i));
    if len(capture) > 0:
        return capture[0] 
    return ""

def capF(i):
    cap = i + 1
    if i >= 3:
        cap += 1
    return(cap)

def doMax(size, m):
    sizes[size] = max(sizes[size], len(getCapture(m, size)))

for line in lines:
    m = parseLine.match(line)
    if m:
        for i in range(0, len(sizes)):
            doMax(i, m)

def resized(m, i):
    return(getCapture(m, i).ljust(sizes[i]))

aligned = []

for line in lines:
    m = parseLine.match(line)

    newLine = line
    if m:
        newLine = "\tQ_PROPERTY(" + resized(m, 0) + " " + resized(m, 1) + " READ " + resized(m, 2)
        if len(m.captures(5)) > 0:
            newLine += " WRITE " + resized(m, 3)
        else:
            newLine += "".ljust(len(" WRITE ") + sizes[3])

        newLine += " NOTIFY " + resized(m, 4) + " )" + ("" if len(m.captures(7)) == 0 else m.captures(7)[0]) + "\n"
    #print(newLine)        
    aligned.append(newLine)
        
out = open(fileOut, mode='w')

out.writelines(aligned)


#Q_PROPERTY(QColor1 white2 READ white3 WRITE setWhite4 NOTIFY whiteChanged5)