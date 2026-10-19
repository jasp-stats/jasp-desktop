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

#ifndef COLUMNENCODERCONTEXT_H
#define COLUMNENCODERCONTEXT_H

#include "columnencoder.h"

class ColumnEncoderContext
{
public:
	static constexpr int Version = 1;

	ColumnEncoderContext() = default;
	ColumnEncoderContext(const ColumnEncoder::colTypeMap & columns, const ColumnEncoder::colTypeMap & extra);

	static ColumnEncoderContext	fromJson(const Json::Value & context);
	static ColumnEncoderContext	fromJsonString(const char * contextJson);

	Json::Value					toJson() const;

	const ColumnEncoder::colTypeMap&	columns() const		{ return _columns; }
	const ColumnEncoder::colTypeMap&	extra() const		{ return _extra; }
	bool								supplied() const	{ return _supplied; }

private:
	ColumnEncoder::colTypeMap	_columns;
	ColumnEncoder::colTypeMap	_extra;
	bool						_supplied = false;
};

class ScopedColumnEncoderContext
{
public:
	ScopedColumnEncoderContext(const ColumnEncoderContext & context, ColumnEncoder & extraEncoder);
	~ScopedColumnEncoderContext();

private:
	bool						_supplied = false;
	ColumnEncoder				& _extraEncoder;
	ColumnEncoder::colTypeMap	_previousColumns;
	ColumnEncoder::colTypeMap	_previousExtra;
};

Json::Value decodeColumnJson(const char * payloadJson, const char * encoderContextJson, ColumnEncoder & extraEncoder, bool replaceNames = true);

#endif // COLUMNENCODERCONTEXT_H
