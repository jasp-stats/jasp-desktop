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

static Json::Value parsePayloadJson(const char * payloadJson)
{
	if(!payloadJson)
		throw std::runtime_error("Cannot decode column text from a null JSON payload.");

	Json::Value payload;
	Json::Reader reader;
	if(!reader.parse(payloadJson, payload))
		throw std::runtime_error("Could not parse column text JSON payload.");

	return payload;
}

ColumnEncoderContext::ColumnEncoderContext(const ColumnEncoder::colTypeMap & columns, const ColumnEncoder::colTypeMap & extra,
                                           const std::string & prefix)
	: _columns(columns), _extra(extra), _prefix(prefix), _supplied(true)
{
}

ColumnEncoderContext ColumnEncoderContext::fromJson(const Json::Value & context)
{
	if(!context.isObject())
		throw std::runtime_error("Column encoder context must be a JSON object.");

	if(!context.isMember("version") || !context["version"].isInt())
		throw std::runtime_error("Column encoder context must contain an integer version.");
	if(context["version"].asInt() < 1 || context["version"].asInt() > Version)
		throw std::runtime_error("Unsupported column encoder context version.");
	// v1 and v2 share one wire shape (the replay semantics moved into the table, not the JSON):
	// a v1 context (no prefix or a dataset prefix) parses and replays exactly like v2's, so no
	// migration exists and none is needed (dev-branch fixtures: regenerate, don't compat).

	return ColumnEncoderContext(
		columnTypesFromJson(context["columns"], "columns"),
		columnTypesFromJson(context["extra"], "extra"),
		context["prefix"].isString() ? context["prefix"].asString() : std::string()
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
	if(!_prefix.empty())
		context["prefix"] = _prefix;

	return context;
}

ScopedColumnEncoderContext::ScopedColumnEncoderContext(const ColumnEncoderContext & context, ColumnEncoder & extraEncoder)
	: _supplied(context.supplied())
{
	if(!_supplied)
		return;

	// v2 replay: the captured column set becomes a SCOPED block of the one global
	// decode table - nothing else. The live canonical mint, every filter view and the extras
	// encoder are never mutated (the v1 swap mutated whatever the current encoder pointed at,
	// which after Phase 1 is a filter view funneling into the live dataset's mint - the landmine
	// this rework removes). A context that carries its D<ds>_F<f> prefix replays in exactly that
	// namespace; a prefix-less (v1) context replays under the namespace the process-global
	// encoder holds right now, like before.
	std::string columnsPrefix = context.prefix();

	if(columnsPrefix.empty())
		columnsPrefix = ColumnEncoder::columnEncoder()->encodePrefix();

	_columnsBlock.emplace(columnsPrefix, context.columns());
	_extraBlock.emplace(extraEncoder.encodePrefix(), context.extra());

	// Both blocks write themselves on entry and undo themselves (exactly, via their displaced-
	// entry logs) in the destructor - member order makes the extra block undone first, which is
	// irrelevant as they never share a namespace.
}

ScopedColumnEncoderContext::~ScopedColumnEncoderContext() = default;

Json::Value decodeColumnJson(const char * payloadJson, const char * encoderContextJson, ColumnEncoder & extraEncoder, bool replaceNames)
{
	Json::Value payload = parsePayloadJson(payloadJson);
	ColumnEncoderContext context = ColumnEncoderContext::fromJsonString(encoderContextJson);
	ScopedColumnEncoderContext scopedContext(context, extraEncoder);

	ColumnEncoder::decodeJson(payload, replaceNames);	//context-free global table

	return payload;
}
