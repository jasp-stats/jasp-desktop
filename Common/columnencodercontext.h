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
	ColumnEncoderContext(const ColumnEncoder::colTypeMap & columns, const ColumnEncoder::colTypeMap & extra,
	                     const std::string & prefix = std::string());

	static ColumnEncoderContext	fromJson(const Json::Value & context);
	static ColumnEncoderContext	fromJsonString(const char * contextJson);

	Json::Value					toJson() const;

	const ColumnEncoder::colTypeMap&	columns() const		{ return _columns; }
	const ColumnEncoder::colTypeMap&	extra() const		{ return _extra; }
	///< The encode namespace (prefix) the captured columns were minted in ("" = v1 context,
	///< replay under whatever prefix is live). DataSet encoders mint per-dataset prefixed names
	///< (JASPColumn_<dataSetId>_), so a context captured on one dataset must carry its prefix
	///< to replay correctly once another dataset is live.
	const std::string&					prefix() const		{ return _prefix; }
	bool								supplied() const	{ return _supplied; }

private:
	ColumnEncoder::colTypeMap	_columns;
	ColumnEncoder::colTypeMap	_extra;
	std::string					_prefix;
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
	std::string					_previousPrefix;
};

Json::Value decodeColumnJson(const char * payloadJson, const char * encoderContextJson, ColumnEncoder & extraEncoder, bool replaceNames = true);

#endif // COLUMNENCODERCONTEXT_H
