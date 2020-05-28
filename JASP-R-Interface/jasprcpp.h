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

#include <RInside/RInside.h>
#include <Rcpp.h>
#include "jasprcpp_interface.h"
#include "columntype.h"

// Calls From R
Rcpp::DataFrame jaspRCPP_readFullDataSet();
Rcpp::DataFrame jaspRCPP_readFilterDataSet();
Rcpp::DataFrame jaspRCPP_readDataSetSEXP(		SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame jaspRCPP_readDataSetHeaderSEXP(	SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(const RBridgeColumn* colResults, size_t colMax);

SEXP jaspRCPP_callbackSEXP(SEXP results, SEXP progress);
SEXP jaspRCPP_requestSpecificFileNameSEXP(SEXP extension);
SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension);
SEXP jaspRCPP_requestTempRootNameSEXP();
SEXP jaspRCPP_requestStateFileNameSEXP();
SEXP jaspRCPP_RunSeparateR(SEXP code);

				//Custom parseEvals to make sure sink is set (to capture output)
void			jaspRCPP_parseEvalQNT(const std::string & code);
RInside::Proxy	jaspRCPP_parseEval(const std::string & code);
void			jaspRCPP_logString(const std::string & code);
SEXP			jaspRCPP_CreateCaptureConnection();

bool jaspRCPP_requestSpecificRelativeFilePath(std::string specificFilename, std::string & root, std:: string & relativePath);
bool jaspRCPP_requestJaspResultsRelativeFilePath(							std::string & root, std:: string & relativePath);

void jaspRCPP_returnDataFrame(Rcpp::DataFrame frame);
void jaspRCPP_returnString(SEXP Message);
void jaspRCPP_setRWarning(SEXP Message);
void jaspRCPP_setRError(SEXP Message);
void jaspRCPP_setLog(SEXP Message);

std::string jaspRCPP_encodeColumnName(		std::string in);
std::string jaspRCPP_decodeColumnName(		std::string in);
std::string jaspRCPP_encodeAllColumnNames(	std::string in);
std::string jaspRCPP_decodeAllColumnNames(	std::string in);


int jaspRCPP_dataSetRowCount();

bool jaspRCPP_columnIsScale(				std::string columnName	);
bool jaspRCPP_columnIsOrdinal(				std::string columnName		  );
bool jaspRCPP_columnIsNominal(				std::string columnName				 );
bool jaspRCPP_columnIsNominalText(			std::string columnName						);

bool jaspRCPP_setColumnDataAsScale(			std::string columnName,	Rcpp::RObject scalarData	);
bool jaspRCPP_setColumnDataAsOrdinal(		std::string columnName,	Rcpp::RObject ordinalData		);
bool jaspRCPP_setColumnDataAsNominal(		std::string columnName,	Rcpp::RObject nominalData			);
bool jaspRCPP_setColumnDataAsNominalText(	std::string columnName,	Rcpp::RObject nominalData			);

bool _jaspRCPP_setColumnDataAsScale(		std::string columnName,	Rcpp::Vector<REALSXP> scalarData	);
bool _jaspRCPP_setColumnDataAsOrdinal(		std::string columnName,	Rcpp::Vector<INTSXP> ordinalData	);
bool _jaspRCPP_setColumnDataAsNominal(		std::string columnName,	Rcpp::Vector<INTSXP> nominalData	);
bool _jaspRCPP_setColumnDataAsNominalText(	std::string columnName,	Rcpp::Vector<STRSXP> nominalData	);

void jaspRCPP_setColumnDataHelper_FactorsLevels(Rcpp::Vector<INTSXP> data, int *& outputData, size_t & numLevels, const char **& labelPointers, std::string *& labels);

//Calls from JASPresult (from R)
typedef void (*sendFuncDef)(const char *);

RBridgeColumnType* jaspRCPP_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, size_t * colMax);

Rcpp::IntegerVector jaspRCPP_makeFactor(Rcpp::IntegerVector v, char** levels, int nbLevels, bool ordinal = false);
void freeRBridgeColumnType(RBridgeColumnType* columnsRequested, size_t colMax);

std::string _jaspRCPP_System(std::string cmd);
void jaspRCPP_postProcessLocalPackageInstall(SEXP moduleLibFileNames);

columnType jaspRCPP_getColumnType(std::string columnName);

void jaspRCPP_crashPlease();
void jaspRCPP_checkForCrashRequest();


std::string __sinkMe(const std::string code);

#endif // JASPRCPP_H
