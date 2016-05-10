//
// Copyright (C) 2015-2016 University of Amsterdam
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

#ifndef MEASURES_H
#define MEASURES_H

namespace spss
{

/*
 * SPSS uses this metric mewaue to deterime both data type
 * double / string and the type of the number, Ornial Cardial etc.
 */
class Measures
{
public:
	enum e_measures
	{
		string_type = -2,
		measure_undefined = -1,
		measure_spss_unknown = 0,
		measure_nominal = 1,
		measure_ordinal = 2,
		measure_continuous = 3
	};
};
}// End namespace spss

#endif // MEASURES_H
