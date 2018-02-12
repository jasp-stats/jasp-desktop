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
Rcpp::DataFrame jaspRCPP_convertRBridgeColumns_to_DataFrame(RBridgeColumn* colResults, int colMax);

SEXP jaspRCPP_callbackSEXP(SEXP results, SEXP progress);
SEXP jaspRCPP_requestTempFileNameSEXP(SEXP extension);
SEXP jaspRCPP_requestStateFileNameSEXP();

void jaspRCPP_returnDataFrame(Rcpp::DataFrame frame);
void jaspRCPP_returnString(SEXP Message);

// This is a copy of column.h!!!!
enum ColumnType { ColumnTypeUnknown = -1, ColumnTypeScale = 0, ColumnTypeOrdinal, ColumnTypeNominal, ColumnTypeNominalText};

RBridgeColumnType* jaspRCPP_marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns, int* colMax);

void jaspRCPP_makeFactor(Rcpp::IntegerVector &v, char** levels, int nbLevels, bool ordinal = false);
void freeRBridgeColumnType(RBridgeColumnType* columnsRequested, int colMax);



#endif // JASPRCPP_H
