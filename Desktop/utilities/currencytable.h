//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#ifndef CURRENCYTABLE_H
#define CURRENCYTABLE_H

#include <QString>
#include <QStringList>

///ISO 4217 currency codes and the symbols they are usually written with, used by the currency override in the
///language preferences (see LanguageModel): to validate a chosen code, to fill in the symbol belonging to a code
///and to find a code belonging to a symbol. The pre-euro currencies are in there as well, because auditors
///regularly need to display amounts in those.
///Symbols are not unique ("$" stands for about twenty codes, "kr" for the Scandinavian ones), so codeForSymbol()
///returns the *first* entry with that symbol: the order of the table is the reverse-lookup priority. Deriving the
///default of a locale does not go through this table at all: QLocale hands out the ISO code of its own currency.
namespace CurrencyTable
{
	QStringList	allCodes();
	bool		isValidCode(const QString & code);
	QString		symbolForCode(const QString & code);	///< The usual symbol of the code, "" when it is unknown
	QString		codeForSymbol(const QString & symbol);	///< The first code that goes by the symbol, "" when none does
}

#endif // CURRENCYTABLE_H
