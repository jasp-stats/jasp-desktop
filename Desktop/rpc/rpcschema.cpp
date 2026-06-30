//
// RpcSchema — JSON-Schema / OpenRPC method-spec types.
//
// Contains the fromJson / toJson factories and serialisers for:
//   RpcSchema, RpcParamSpec, RpcResultSpec, RpcMethodSpec
//

#include "rpcschema.h"

#include "json/json.h"
#include <stdexcept>

// =========================================================================
//  Free helper — applySchemaDefaults
// =========================================================================

/// Recursively apply property defaults inside an object value.
Json::Value applySchemaDefaults(const Json::Value& value,
								const RpcSchema& schema)
{
	if (!value.isObject() || schema.properties.empty())
		return value;

	Json::Value out = value;
	for (const auto& prop : schema.properties)
	{
		if (!out.isMember(prop.name) && !prop.defaultValue.isNull())
			out[prop.name] = prop.defaultValue;

		if (out.isMember(prop.name) && prop.schema)
			out[prop.name] = applySchemaDefaults(out[prop.name], *prop.schema);
	}
	return out;
}

// =========================================================================
//  RpcSchema
// =========================================================================

RpcSchema RpcSchema::fromJson(const Json::Value& json)
{
	RpcSchema s;
	s.type         = json.get("type", "").asString();
	s.description  = json.get("description", "").asString();
	s.defaultValue = json.get("default", Json::nullValue);

	// ---- required list (only meaningful for objects) ------------------
	if (json.isMember("required") && json["required"].isArray())
		for (const auto& r : json["required"])
			s.required.push_back(r.asString());

	// ---- properties ---------------------------------------------------
	if (json.isMember("properties") && json["properties"].isObject())
	{
		const Json::Value& props = json["properties"];
		for (const auto& name : props.getMemberNames())
		{
			Property prop;
			prop.name         = name;
			prop.description  = props[name].get("description", "").asString();
			prop.defaultValue = props[name].get("default",     Json::nullValue);

			// Per-property required flag: true if name appears in
			// the parent's "required" array.
			for (const auto& req : s.required)
				if (req == name) { prop.required = true; break; }

			// Recurse: the property value may itself be a schema object.
			const Json::Value& propJson = props[name];
			if (propJson.isMember("type") || propJson.isMember("properties") ||
				propJson.isMember("required"))
			{
				prop.schema = std::make_unique<RpcSchema>(RpcSchema::fromJson(propJson));
			}

			s.properties.push_back(std::move(prop));
		}
	}

	return s;
}

RpcSchema RpcSchema::any()
{
	RpcSchema s;
	s.type = "any";
	return s;
}

Json::Value RpcSchema::toJson() const
{
	Json::Value j;
	if (!type.empty())        j["type"]        = type;
	if (!description.empty()) j["description"] = description;
	if (!defaultValue.isNull()) j["default"]   = defaultValue;
	if (!required.empty())
	{
		Json::Value req(Json::arrayValue);
		for (const auto& r : required) req.append(r);
		j["required"] = req;
	}
	if (!properties.empty())
	{
		Json::Value props(Json::objectValue);
		for (const auto& pr : properties)
		{
			Json::Value pj;
			if (!pr.description.empty())  pj["description"] = pr.description;
			if (!pr.defaultValue.isNull()) pj["default"]     = pr.defaultValue;
			if (pr.schema)                pj = pr.schema->toJson();
			props[pr.name] = pj;
		}
		j["properties"] = props;
	}
	return j;
}

// =========================================================================
//  RpcMethodSpec
// =========================================================================

RpcMethodSpec RpcMethodSpec::fromJson(const Json::Value& json)
{
	if (!json.isObject())
		throw std::runtime_error("RpcMethodSpec must be a JSON object");

	RpcMethodSpec spec;

	// ---- name ---------------------------------------------------------
	if (!json.isMember("name") || !json["name"].isString())
		throw std::runtime_error("RpcMethodSpec: missing 'name' (string)");
	spec.name = json["name"].asString();

	// ---- summary ------------------------------------------------------
	spec.summary    = json.get("summary", "").asString();
	spec.displayName = json.get("x-displayName", "").asString();
	spec.failOnStateDiverged = json.get("x-failOnStateDiverged", false).asBool();

	// ---- params -------------------------------------------------------
	if (!json.isMember("params") || !json["params"].isArray())
		throw std::runtime_error("RpcMethodSpec '" + spec.name +
								 "': missing 'params' (array)");

	for (const auto& pJson : json["params"])
	{
		if (!pJson.isObject())
			throw std::runtime_error("RpcMethodSpec '" + spec.name +
									 "': each param must be an object");

		RpcParamSpec p;
		if (!pJson.isMember("name") || !pJson["name"].isString())
			throw std::runtime_error("RpcMethodSpec '" + spec.name +
									 "': param missing 'name'");
		p.name        = pJson["name"].asString();
		p.description = pJson.get("description", "").asString();
		p.required    = pJson.get("required", true).asBool();

		if (pJson.isMember("schema"))
			p.schema = RpcSchema::fromJson(pJson["schema"]);
		// else: RpcSchema default = any type accepted

		spec.params.push_back(std::move(p));
	}

	// ---- result -------------------------------------------------------
	if (json.isMember("result") && json["result"].isObject())
	{
		const Json::Value& rJson = json["result"];
		spec.result.name        = rJson.get("name", "").asString();
		spec.result.description = rJson.get("description", "").asString();
		if (rJson.isMember("schema"))
			spec.result.schema = RpcSchema::fromJson(rJson["schema"]);
	}

	return spec;
}

RpcMethodSpec RpcMethodSpec::fromJsonString(const std::string& jsonStr)
{
	Json::Value  root;
	Json::Reader reader;
	if (!reader.parse(jsonStr, root))
		throw std::runtime_error(
			"RpcMethodSpec parse error: " + reader.getFormattedErrorMessages());
	return fromJson(root);
}

Json::Value RpcMethodSpec::toJson() const
{
	Json::Value m;
	m["name"]    = name;
	m["summary"] = summary;
	if (!displayName.empty()) m["x-displayName"] = displayName;
	Json::Value plist(Json::arrayValue);
	for (const auto& p : params)
	{
		Json::Value pj;
		pj["name"]        = p.name;
		pj["required"]    = p.required;
		pj["description"] = p.description;
		pj["schema"]      = p.schema.toJson();
		plist.append(pj);
	}
	m["params"] = plist;
	Json::Value r;
	r["name"]        = result.name;
	r["description"] = result.description;
	r["schema"]      = result.schema.toJson();
	m["result"] = r;
	return m;
}