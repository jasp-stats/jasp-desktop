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

#include "columnencodercontext.h"

#include <stdexcept>

static Json::Value columnTypesToJson(const ColumnEncoder::colTypeMap & columnTypes)
{
	Json::Value columns(Json::arrayValue);
	for(const auto & nameType : columnTypes)
	{
		Json::Value column(Json::objectValue);
		column["name"] = nameType.first;
		column["type"] = columnTypeToString(nameType.second);
		columns.append(column);
	}

	return columns;
}

static ColumnEncoder::colTypeMap columnTypesFromJson(const Json::Value & columns, const char * fieldName)
{
	ColumnEncoder::colTypeMap columnTypes;

	if(columns.isNull())
		return columnTypes;
	if(!columns.isArray())
		throw std::runtime_error(std::string("Column encoder context field '") + fieldName + "' must be an array.");

	for(const Json::Value & column : columns)
	{
		if(!column.isObject() || !column["name"].isString() || !column["type"].isString())
			throw std::runtime_error(std::string("Column encoder context field '") + fieldName + "' must contain objects with string 'name' and 'type' fields.");

		columnTypes[column["name"].asString()] = columnTypeFromString(column["type"].asString());
	}

	return columnTypes;
}

static Json::Value parseStringArrayJson(const char * valuesJson)
{
	if(!valuesJson)
		throw std::runtime_error("Cannot decode column text from a null JSON payload.");

	Json::Value values;
	Json::Reader reader;
	if(!reader.parse(valuesJson, values))
		throw std::runtime_error("Could not parse column text JSON payload.");
	if(!values.isArray())
		throw std::runtime_error("Column text JSON payload must be an array.");

	return values;
}

ColumnEncoderContext::ColumnEncoderContext(const ColumnEncoder::colTypeMap & columns, const ColumnEncoder::colTypeMap & extra)
	: _columns(columns), _extra(extra), _supplied(true)
{
}

ColumnEncoderContext ColumnEncoderContext::fromJson(const Json::Value & context)
{
	if(!context.isObject())
		throw std::runtime_error("Column encoder context must be a JSON object.");

	if(!context.isMember("version") || !context["version"].isInt())
		throw std::runtime_error("Column encoder context must contain integer version 1.");
	if(context["version"].asInt() != Version)
		throw std::runtime_error("Unsupported column encoder context version.");

	return ColumnEncoderContext(
		columnTypesFromJson(context["columns"], "columns"),
		columnTypesFromJson(context["extra"], "extra")
	);
}

ColumnEncoderContext ColumnEncoderContext::fromJsonString(const char * contextJson)
{
	if(!contextJson || std::string(contextJson).empty())
		return ColumnEncoderContext();

	Json::Value context;
	Json::Reader reader;
	if(!reader.parse(contextJson, context))
		throw std::runtime_error("Could not parse column encoder context JSON.");

	return fromJson(context);
}

Json::Value ColumnEncoderContext::toJson() const
{
	Json::Value context(Json::objectValue);
	context["version"] = Version;
	context["columns"] = columnTypesToJson(_columns);
	context["extra"] = columnTypesToJson(_extra);

	return context;
}

ScopedColumnEncoderContext::ScopedColumnEncoderContext(const ColumnEncoderContext & context, ColumnEncoder & extraEncoder)
	: _supplied(context.supplied()), _extraEncoder(extraEncoder)
{
	if(!_supplied)
		return;

	_previousColumns = ColumnEncoder::columnEncoder()->currentNames();
	_previousExtra = _extraEncoder.currentNames();

	ColumnEncoder::columnEncoder()->setCurrentNames(context.columns());
	_extraEncoder.setCurrentNames(context.extra());
}

ScopedColumnEncoderContext::~ScopedColumnEncoderContext()
{
	if(!_supplied)
		return;

	ColumnEncoder::columnEncoder()->setCurrentNames(_previousColumns);
	_extraEncoder.setCurrentNames(_previousExtra);
}

Json::Value decodeColumnTextJson(const char * valuesJson, const char * encoderContextJson, ColumnEncoder & extraEncoder)
{
	Json::Value values = parseStringArrayJson(valuesJson);
	ColumnEncoderContext context = ColumnEncoderContext::fromJsonString(encoderContextJson);
	Json::Value decodedValues(Json::arrayValue);
	ScopedColumnEncoderContext scopedContext(context, extraEncoder);

	for(const Json::Value & value : values)
	{
		if(value.isNull())
		{
			decodedValues.append(Json::Value());
		}
		else if(value.isString())
		{
			decodedValues.append(ColumnEncoder::decodeAll(value.asString()));
		}
		else
		{
			throw std::runtime_error("Column text JSON payload must contain only strings or null values.");
		}
	}

	return decodedValues;
}
