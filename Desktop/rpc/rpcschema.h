//
// RpcSchema — OpenRPC-inspired schema types for method parameters and results.
//
// These types describe the shape of JSON values accepted by or returned from
// RPC methods.  They can be built programmatically or parsed from JSON
// (e.g. an OpenRPC document loaded at startup).  The dispatcher uses them
// to validate incoming params, fill in declared defaults, and validate
// handler return values before sending responses to the client.
//
// Separated from jasprpcdispatcher.h to keep the dispatcher itself focused
// on registration and dispatch logic.
//

#ifndef JASPRPCSCHEMA_H
#define JASPRPCSCHEMA_H

#include <memory>
#include <string>
#include <vector>

#include "json/json.h"

// =========================================================================
//  RpcSchema — a JSON-Schema fragment (recursive for objects)
// =========================================================================

/// A JSON Schema fragment describing a single value.
/// When type == "object", `properties` and `required` come into play.
struct RpcSchema
{
	std::string   type;          // "string","integer","number","boolean","object","array","null","any"
	std::string   description;   // human-readable
	Json::Value   defaultValue;  // Json::nullValue = no default

	/// Only meaningful when type == "object".
	std::vector<std::string> required;

	/// One named property inside an object schema.
	struct Property
	{
		Property() = default;
		Property(Property&&) = default;
		Property& operator=(Property&&) = default;

		Property(const Property& o)
			: name(o.name), description(o.description)
			, required(o.required), defaultValue(o.defaultValue)
			, schema(o.schema ? std::make_unique<RpcSchema>(*o.schema) : nullptr)
		{}

		Property& operator=(const Property& o)
		{
			if (this != &o)
			{
				name         = o.name;
				description  = o.description;
				required     = o.required;
				defaultValue = o.defaultValue;
				schema       = o.schema ? std::make_unique<RpcSchema>(*o.schema) : nullptr;
			}
			return *this;
		}

		std::string                 name;
		std::string                 description;
		bool                        required     = false;
		Json::Value                 defaultValue;
		std::unique_ptr<RpcSchema>  schema;  // nullptr = accept any value
	};

	std::vector<Property> properties;

	// ---- factories ----------------------------------------------------
	static RpcSchema fromJson(const Json::Value& json);
	static RpcSchema any();           // accepts everything
	Json::Value       toJson() const;
};

// =========================================================================
//  RpcParamSpec — describes one named parameter of an RPC method
// =========================================================================

struct RpcParamSpec
{
	std::string name;
	std::string description;
	bool        required = true;
	RpcSchema   schema;          // RpcSchema::any() = no type check
};

// =========================================================================
//  RpcResultSpec — describes the return value of an RPC method
// =========================================================================

struct RpcResultSpec
{
	std::string name;
	std::string description;
	RpcSchema   schema;
};

// =========================================================================
//  RpcMethodSpec — full method descriptor (OpenRPC-inspired)
// =========================================================================

/// Can be built directly in C++ or parsed from a JSON string / Json::Value.
struct RpcMethodSpec
{
	std::string               name;
	std::string               summary;
	std::string               displayName;  // x-displayName from OpenRPC extension
	bool                      failOnStateDiverged = false; // x-failOnStateDiverged
	std::vector<RpcParamSpec> params;
	RpcResultSpec             result;

	// ---- factories ----------------------------------------------------

	/// Parse a JSON object that follows the OpenRPC method-spec shape.
	/// Throws std::runtime_error on malformed input.
	static RpcMethodSpec fromJson(const Json::Value& json);

	/// Convenience: parse from a raw JSON string.
	static RpcMethodSpec fromJsonString(const std::string& jsonStr);

	Json::Value toJson() const;
};

// =========================================================================
//  Free helpers (used by the dispatcher)
// =========================================================================

/// Recursively fill in declared default values for missing properties inside
/// an object value, according to its RpcSchema.
/// Returns the value unchanged if it isn't an object or the schema has no
/// property defaults.
Json::Value applySchemaDefaults(const Json::Value& value,
                                const RpcSchema&   schema);

#endif // JASPRPCSCHEMA_H