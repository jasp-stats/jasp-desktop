//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef JASPRCPP_H
#define JASPRCPP_H

#include <RInside.h>
#include <Rcpp.h>
#include "jasprcpp_interface.h"
#include "columntype.h"

/// Main definitions for jaspRcpp functions, only for internal use in R-Interface so stdlib can be used to your hearts delight
/// A lot of these functions are directly callable from R as long as they are registered through RInside, see jaspRCPP_init for that

// Calls From R
Rcpp::DataFrame jaspRCPP_readFullDataSet();
Rcpp::DataFrame jaspRCPP_readFullFilteredDataSet();
Rcpp::DataFrame jaspRCPP_readFilterDataSet();
Rcpp::DataFrame jaspRCPP_readDataSetRequested();
Rcpp::DataFrame jaspRCPP_readDataSetSEXP(		SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame jaspRCPP_readDataSetHeaderSEXP(	SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(const RBridgeColumn* colResults, size_t colMax);

SEXP jaspRCPP_callbackSEXP(SEXP results, SEXP progress);
SEXP jaspRCPP_requestSpecificFileNameSEXP(SEXP extension);
SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension);
SEXP jaspRCPP_requestTempRootNameSEXP();
SEXP jaspRCPP_requestStateFileNameSEXP();
SEXP jaspRCPP_allColumnNamesDataset();
SEXP jaspRCPP_RunSeparateR(SEXP code);

				//Custom parseEvals to make sure sink is set (to capture output)
void			jaspRCPP_parseEvalQNT(			const std::string & code,	bool setWd = false,	bool preface = true);
RInside::Proxy	jaspRCPP_parseEval(				const std::string & code,	bool setWd = false,	bool preface = true);
std::string		jaspRCPP_parseEvalStringReturn(	const std::string & code,	bool setWd = false, bool preface = true);

void			jaspRCPP_logString(		const std::string & code);
SEXP			jaspRCPP_CreateCaptureConnection();

bool jaspRCPP_requestSpecificRelativeFilePath(std::string specificFilename, std::string & root, std:: string & relativePath);
bool jaspRCPP_requestJaspResultsRelativeFilePath(							std::string & root, std:: string & relativePath);

void jaspRCPP_returnDataFrame(Rcpp::DataFrame frame);
void jaspRCPP_returnString(SEXP Message);
void jaspRCPP_setRWarning(SEXP Message);
void jaspRCPP_setRError(SEXP Message);
void jaspRCPP_setLog(SEXP Message);

Rcpp::String	jaspRCPP_encodeColumnNameRcpp(	const	Rcpp::String & in);
Rcpp::String	jaspRCPP_decodeColumnNameRcpp(	const	Rcpp::String & in);
Rcpp::String	jaspRCPP_decodeColumnTypeRcpp(	const	Rcpp::String & in);
std::string		jaspRCPP_encodeColumnName(				std::string    in);
std::string		jaspRCPP_decodeColumnName(				std::string    in);
Rcpp::String	jaspRCPP_encodeAllColumnNames(	const	Rcpp::String & in);
Rcpp::String	jaspRCPP_decodeAllColumnNames(	const	Rcpp::String & in);
bool			jaspRCPP_shouldEncodeColumnName(		std::string    in);
bool			jaspRCPP_shouldDecodeColumnName(		std::string    in);
std::string		jaspRCPP_nativeToUtf8(			const	Rcpp::String & in);


int jaspRCPP_dataSetRowCount();

bool jaspRCPP_columnIsScale(				const std::string & columnName	);
bool jaspRCPP_columnIsOrdinal(				const std::string & columnName		  );
bool jaspRCPP_columnIsNominal(				const std::string & columnName				 );

bool jaspRCPP_setColumnDataAsScale(			const std::string & columnName,	Rcpp::RObject scalarData	);
bool jaspRCPP_setColumnDataAsOrdinal(		const std::string & columnName,	Rcpp::RObject ordinalData		);
bool jaspRCPP_setColumnDataAsNominal(		const std::string & columnName,	Rcpp::RObject nominalData			);
bool _jaspRCPP_setColumnDataAndType(		const std::string & columnName, Rcpp::RObject data, columnType colType);

void jaspRCPP_setColumnDataHelper_FactorsLevels(Rcpp::Vector<INTSXP> data, int *& outputData, size_t & numLevels, const char **& labelPointers, std::string *& labels);

//Calls from JASPresult (from R)
typedef void (*sendFuncDef)(const char *);

//Calls from jaspBase
typedef void			(*logFuncDef)(const std::string &);
typedef bool			(*shouldEnDecodeFuncDef)	(std::string);
typedef bool			(*setColumnDataFuncDef)		(const std::string &, Rcpp::RObject);
typedef columnType		(*getColumnTypeFuncDef)		(std::string);
typedef int				(*getColumnAnIdFuncDef)		(std::string);
typedef bool			(*getColumnExistsFDef)		(std::string);
typedef std::string		(*createColumnFuncDef)		(std::string);
typedef bool			(*deleteColumnFuncDef)		(std::string);
typedef std::string		(*enDecodeFuncDef)			(std::string);

void					freeRBridgeColumnType(	RBridgeColumnType* columnsRequested, size_t colMax);

RBridgeColumnType*		jaspRCPP_marshallSEXPs(			SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, size_t * colMax);
Rcpp::IntegerVector		jaspRCPP_makeFactor(			Rcpp::IntegerVector v, char** levels, int nbLevels, bool ordinal = false);
std::string				_jaspRCPP_System (				std::string cmd);
columnType				jaspRCPP_getColumnType(			std::string columnName);
bool					jaspRCPP_getColumnExists(		std::string columnName);
int						jaspRCPP_getColumnAnalysisId(	std::string columnName);
std::string				jaspRCPP_createColumn(			std::string columnName);
bool					jaspRCPP_deleteColumn(			std::string columnName);
void					jaspRCPP_crashPlease();
void					jaspRCPP_checkForCrashRequest();
void					jaspRCPP_postProcessLocalPackageInstall(	SEXP moduleLibFileNames);

std::string				__sinkMe(const std::string code = "");

#endif // JASPRCPP_H
