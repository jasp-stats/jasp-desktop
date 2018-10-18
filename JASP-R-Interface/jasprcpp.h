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

// Calls From R
Rcpp::DataFrame jaspRCPP_readFullDataSet();
Rcpp::DataFrame jaspRCPP_readFilterDataSet();
Rcpp::DataFrame jaspRCPP_readDataSetSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame jaspRCPP_readDataSetHeaderSEXP(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(const RBridgeColumn* colResults, int colMax);

SEXP jaspRCPP_callbackSEXP(SEXP results, SEXP progress);
SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension);
SEXP jaspRCPP_requestTempRootNameSEXP();
SEXP jaspRCPP_requestStateFileNameSEXP();

const char * jaspRCPP_requestJaspResultsRelativeFilePath();

void jaspRCPP_returnDataFrame(Rcpp::DataFrame frame);
void jaspRCPP_returnString(SEXP Message);
void jaspRCPP_setRWarning(SEXP Message);
void jaspRCPP_setRError(SEXP Message);
int jaspRCPP_dataSetRowCount();


bool jaspRCPP_setColumnDataAsScale(std::string columnName,			Rcpp::Vector<REALSXP> scalarData);
bool jaspRCPP_setColumnDataAsOrdinal(std::string columnName,		Rcpp::Vector<INTSXP> ordinalData);
bool jaspRCPP_setColumnDataAsNominal(std::string columnName,		Rcpp::Vector<INTSXP> nominalData);
bool jaspRCPP_setColumnDataAsNominalText(std::string columnName,	Rcpp::Vector<STRSXP> nominalData);

//Calls from JASPresult (from R)
typedef void (*sendFuncDef)(const char *);

// This is a copy of column.h!!!!
enum ColumnType { ColumnTypeUnknown = 0, ColumnTypeNominal = 1, ColumnTypeNominalText = 2, ColumnTypeOrdinal = 4, ColumnTypeScale = 8 };

RBridgeColumnType* jaspRCPP_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, int* colMax);

Rcpp::IntegerVector jaspRCPP_makeFactor(Rcpp::IntegerVector v, char** levels, int nbLevels, bool ordinal = false);
void freeRBridgeColumnType(RBridgeColumnType* columnsRequested, int colMax);



#endif // JASPRCPP_H
