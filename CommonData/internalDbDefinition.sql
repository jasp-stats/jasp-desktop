-- This file is automatically converted to an includable string at internalDbDefintion.h for inclusion

CREATE TABLE DataSets ( 
	id				INTEGER PRIMARY KEY, 
	dataFilePath	TEXT, 
	dataFileTimestamp INT DEFAULT 0,
	description		TEXT,
	databaseJson	TEXT, 
	emptyValuesJson TEXT, 
	revision		INT DEFAULT 0, 
	dataFileSynch	INT
);

CREATE TABLE Filters ( 
	id				INTEGER PRIMARY KEY, 
	dataSet			INT, 
	rFilter			TEXT,
	name			TEXT, 
	generatedFilter TEXT, 
	constructorJson TEXT, 
	constructorR	TEXT, 
	errorMsg		TEXT,
	revision		INT DEFAULT 0, 
	
	FOREIGN KEY(dataSet) REFERENCES DataSets(id)
);

CREATE TABLE Columns
( 
	id					INTEGER PRIMARY KEY, 
	dataSet				INT, 
	name				TEXT, 
	title				TEXT, 
	description			TEXT, 
	columnType			TEXT, 
	colIdx				INT, 
	isComputed			INT, 
	autoSortByValue		INT,	
	invalidated			INT		NULL,
	forceSourceColType	INT		NULL,
	codeType			TEXT	NULL, 
	rCode				TEXT	NULL, 
	error				TEXT	NULL, 
	constructorJson		TEXT	NULL, 
	analysisID			INT		NULL, 
	emptyValuesJson		TEXT	NULL,
	revision			INT		DEFAULT 0, 
	
	FOREIGN KEY(dataSet) REFERENCES DataSets(id)
);

CREATE TABLE Labels		
( 
	id					INTEGER PRIMARY KEY, 
	columnId			INT, 
	value				INT, 
	ordering			INT, 
	filterAllows		INT, 
	label				TEXT, 
	originalValueJson	TEXT, 
	description			TEXT, 
	
	FOREIGN KEY(columnId) REFERENCES Columns(id)
);
