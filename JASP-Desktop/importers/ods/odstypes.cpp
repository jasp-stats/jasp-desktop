/*
	Copyright (C) Copyright (C) 2013-2018 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 23-01-2017
	Original file name was
*/

#include "odstypes.h"

using namespace ods;

#ifdef JASP_DEBUG
const char * const ods::ODSTYPE_STR[] =
{
	"odsType_unknown",
	"odsType_float",
	"odsType_currency",
	"odsType_percent",
	"odsType_boolean",
	"odsType_string",
	"odsType_date",
	"odsType_time"
};
#endif
