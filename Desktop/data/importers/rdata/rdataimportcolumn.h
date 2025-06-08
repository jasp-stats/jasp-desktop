//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef RDATAIMPORTCOLUMN_H
#define RDATAIMPORTCOLUMN_H

#include "data/importers/importcolumn.h"

class RDataImportColumn : public ImportColumn
{
public:
	RDataImportColumn(ImportDataSet *importDataSet, std::string name);
	RDataImportColumn(ImportDataSet *importDataSet, std::string name, long reserve);
	~RDataImportColumn() override;

	size_t 			size() 					const override;
	const stringvec allValuesAsStrings() 	const override { return _data; }
	std::string 	valueLookup(size_t row) const override;
	void			addValue(const std::string &value);
	const stringvec &getValues() const;

private:
	stringvec _data;
};

#endif // RDATAIMPORTCOLUMN_H
