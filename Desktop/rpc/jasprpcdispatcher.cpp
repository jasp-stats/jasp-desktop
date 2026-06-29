//
// JaspRpcDispatcher implementation — see jasprpcdispatcher.h for API docs.
//
// Schema serialisation / deserialisation (RpcSchema, RpcMethodSpec, etc.)
// lives in rpcschema.cpp.  This file keeps the dispatcher, its validation
// helpers, and the wrapping logic that ties schemas to handler dispatch.
//

#include "jasprpcdispatcher.h"

#include <cassert>
#include <fstream>
#include <sstream>

#include "dirs.h"
#include "log.h"
#include "ai/agentstatetracker.h"

// =========================================================================
//  Internal helpers (not exposed)
// =========================================================================

namespace
{

const char* jsonTypeName(Json::ValueType t)
{
	switch (t)
	{
	case Json::nullValue:    return "null";
	case Json::intValue:     return "integer";
	case Json::uintValue:    return "integer";
	case Json::realValue:    return "number";
	case Json::stringValue:  return "string";
	case Json::booleanValue: return "boolean";
	case Json::arrayValue:   return "array";
	case Json::objectValue:  return "object";
	}
	return "unknown";
}

bool typeMatches(Json::ValueType actual, const std::string& schemaType)
{
	if (schemaType.empty() || schemaType == "any") return true;

	if (schemaType == "string")  return actual == Json::stringValue;
	if (schemaType == "integer") return actual == Json::intValue  ||
	                                     actual == Json::uintValue;
	if (schemaType == "number")  return actual == Json::intValue  ||
	                                     actual == Json::uintValue ||
	                                     actual == Json::realValue;
	if (schemaType == "boolean") return actual == Json::booleanValue;
	if (schemaType == "object")  return actual == Json::objectValue;
	if (schemaType == "array")   return actual == Json::arrayValue;
	if (schemaType == "null")    return actual == Json::nullValue;

	return true; // unknown type name → permissive
}

/// Returns a JSON error object with code -32602 and the given message.
Json::Value invalidParamsError(const std::string& msg)
{
	Json::Value err;
	err["code"]    = -32602;
	err["message"] = "Invalid params: " + msg;
	return err;
}

} // anonymous namespace

// =========================================================================
//  JaspRpcDispatcher — singleton
// =========================================================================

JaspRpcDispatcher* JaspRpcDispatcher::_singleton = nullptr;

JaspRpcDispatcher::JaspRpcDispatcher()
{
	assert(!_singleton);
	_singleton = this;

	// Auto-load the OpenRPC spec file from the Resources directory.
	std::string specPath = Dirs::resourcesDir() + "JASP_RPC.json";
	int n = loadSpecFile(specPath);
	if (n > 0)
		Log::log() << "[JaspRpcDispatcher] Loaded " << n
				  << " method specs from " << specPath << std::endl;
	else if (n == 0)
		Log::log() << "[" << specPath << "] found but no methods loaded"
				  << std::endl;
	// n < 0 → file not found / parse error; silently OK.

	// Register built-in methods (ping, rpc_discover, etc.)
	registerBuiltins();
}

JaspRpcDispatcher::~JaspRpcDispatcher()
{
	_singleton = nullptr;
}

// =========================================================================
//  Static validation helpers
// =========================================================================

Json::Value JaspRpcDispatcher::validateSchema(const Json::Value& value,
											  const RpcSchema& schema)
{
	// ---- type check ---------------------------------------------------
	if (!schema.type.empty() && schema.type != "any")
	{
		if (!typeMatches(value.type(), schema.type))
		{
			std::ostringstream msg;
			msg << "type mismatch: expected " << schema.type
				<< ", got " << jsonTypeName(value.type());
			return invalidParamsError(msg.str());
		}
	}

	// ---- object: check required properties & recurse ------------------
	if (value.isObject() && !schema.properties.empty())
	{
		for (const auto& prop : schema.properties)
		{
			if (prop.required && !value.isMember(prop.name))
				return invalidParamsError("missing required property: '" +
										  prop.name + "'");

			if (value.isMember(prop.name) && prop.schema)
			{
				Json::Value nested = validateSchema(value[prop.name], *prop.schema);
				if (!nested.isNull())
					return nested; // propagate inner error
			}
		}
	}

	return Json::nullValue;
}

Json::Value JaspRpcDispatcher::validateParams(
	const Json::Value& params,
	const std::vector<RpcParamSpec>& spec)
{
	if (spec.empty())
		return Json::nullValue;

	// Top-level params should be a JSON object (JSON-RPC by-name style).
	if (!params.isObject())
		return invalidParamsError("params must be a JSON object");

	for (const auto& p : spec)
	{
		if (p.required && !params.isMember(p.name))
			return invalidParamsError("missing required param: '" + p.name + "'");

		if (params.isMember(p.name))
		{
			const RpcSchema& sch = p.schema;
			if (!sch.type.empty() || !sch.properties.empty())
			{
				Json::Value err = validateSchema(params[p.name], sch);
				if (!err.isNull())
					return err;
			}
		}
	}

	return Json::nullValue;
}

Json::Value JaspRpcDispatcher::validateResult(const Json::Value& result,
											  const RpcResultSpec& spec)
{
	if (spec.schema.type.empty() && spec.schema.properties.empty())
		return Json::nullValue;

	return validateSchema(result, spec.schema);
}

// =========================================================================
//  Static default-application helpers
// =========================================================================

Json::Value JaspRpcDispatcher::applyDefaults(
	const Json::Value& params,
	const std::vector<RpcParamSpec>& spec)
{
	Json::Value out = params.isObject() ? params : Json::Value(Json::objectValue);

	for (const auto& p : spec)
	{
		// Top-level param default
		if (!out.isMember(p.name) && !p.schema.defaultValue.isNull())
			out[p.name] = p.schema.defaultValue;

		// Recurse into the value if it's an object with property defaults
		if (out.isMember(p.name))
			out[p.name] = applySchemaDefaults(out[p.name], p.schema);
	}

	return out;
}

// =========================================================================
//  Nested event loop helper
// =========================================================================

void JaspRpcDispatcher::waitAndProcessEvents(int timeoutMs,
	std::function<void(QEventLoop&, QTimer&)> setup)
{
	QTimer timer;
	timer.setSingleShot(true);

	QEventLoop loop;

	// Timer timeout always quits — guarantees we never block indefinitely.
	QMetaObject::Connection timerConn = QObject::connect(
		&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

	// Let the caller wire up their condition signals.
	setup(loop, timer);

	timer.start(timeoutMs);
	loop.exec();
	timer.stop();

	QObject::disconnect(timerConn);
}

// =========================================================================
//  Convenience builders
// =========================================================================

Json::Value JaspRpcDispatcher::successResult()
{
	Json::Value r;
	r["status"] = "success";
	return r;
}

Json::Value JaspRpcDispatcher::errorResult(const std::string& message)
{
	Json::Value r;
	r["status"]  = "error";
	r["message"] = message;
	return r;
}

// =========================================================================
//  Registration — bare (no validation)
// =========================================================================

bool JaspRpcDispatcher::registerMethod(const std::string& method,
									   RpcHandler handler)
{
	if (_handlers.find(method) != _handlers.end())
		return false;

	_handlers[method] = std::move(handler);
	return true;
}

// =========================================================================
//  Registration — flat param spec
// =========================================================================

bool JaspRpcDispatcher::registerMethod(const std::string& method,
									   std::vector<RpcParamSpec> paramSpec,
									   RpcHandler handler)
{
	if (_handlers.find(method) != _handlers.end())
		return false;

	auto wrapped = [handler = std::move(handler),
					spec = std::move(paramSpec)](const Json::Value& params) -> Json::Value
	{
		// 1. Validate
		Json::Value err = validateParams(params, spec);
		if (!err.isNull())
			return err;

		// 2. Apply defaults
		Json::Value safeParams = applyDefaults(params, spec);

		// 3. Call handler
		return handler(safeParams);
	};

	_handlers[method] = std::move(wrapped);
	return true;
}

// =========================================================================
//  Registration — full OpenRPC method spec
// =========================================================================

bool JaspRpcDispatcher::registerMethod(const RpcMethodSpec& spec,
									   RpcHandler handler)
{
	if (_handlers.find(spec.name) != _handlers.end())
		return false;

	auto wrapped = [handler = std::move(handler),
					spec](const Json::Value& params) -> Json::Value
	{
		// 1. Validate incoming params against declared schemas
		Json::Value err = validateParams(params, spec.params);
		if (!err.isNull())
			return err;

		// 2. Apply declared default values for missing optional params
		Json::Value safeParams = applyDefaults(params, spec.params);

		// 3. Call the handler with validated, defaulted params
		Json::Value result = handler(safeParams);

		// 4. Validate the handler's return value against result.schema
		//    Skip validation for error responses so the real error is not swallowed.
		if (!(result.isMember("status") && result["status"] == "error"))
		{
			err = validateResult(result, spec.result);
			if (!err.isNull())
				return err;
		}

		return result;
	};

	_handlers[spec.name] = std::move(wrapped);
	return true;
}

bool JaspRpcDispatcher::registerMethodFromSpec(const std::string& specJson,
											   RpcHandler handler)
{
	RpcMethodSpec spec = RpcMethodSpec::fromJsonString(specJson);
	return registerMethod(spec, std::move(handler));
}

bool JaspRpcDispatcher::registerMethodByName(const std::string& methodName,
											 RpcHandler handler)
{
	auto it = _specs.find(methodName);
	if (it == _specs.end())
		return false;

	return registerMethod(it->second, std::move(handler));
}

// =========================================================================
//  Spec-file loading
// =========================================================================

int JaspRpcDispatcher::loadSpecFromString(const std::string& openRpcJson)
{
	Json::Value  root;
	Json::Reader reader;
	if (!reader.parse(openRpcJson, root))
	{
		Log::log() << "[JaspRpcDispatcher] Failed to parse spec JSON: "
				  << reader.getFormattedErrorMessages() << std::endl;
		return -1;
	}

	// OpenRPC 1.x: top-level "methods" array.
	if (!root.isMember("methods") || !root["methods"].isArray())
	{
		Log::log() << "[JaspRpcDispatcher] Spec JSON missing 'methods' array"
				  << std::endl;
		return 0;
	}

	int loaded = 0;
	for (const auto& mJson : root["methods"])
	{
		if (!mJson.isObject())
			continue;

		try
		{
			RpcMethodSpec spec = RpcMethodSpec::fromJson(mJson);
			std::string name = spec.name;
			_specs[name] = std::move(spec);
			++loaded;
		}
		catch (const std::exception& e)
		{
			Log::log() << "[JaspRpcDispatcher] Skipping malformed method spec: "
					  << e.what() << std::endl;
		}
	}

	return loaded;
}

int JaspRpcDispatcher::loadSpecFile(const std::string& path)
{
	std::ifstream file(path);
	if (!file.is_open())
		return -1;

	std::string content(
		(std::istreambuf_iterator<char>(file)),
		std::istreambuf_iterator<char>());

	return loadSpecFromString(content);
}

std::vector<std::string> JaspRpcDispatcher::knownSpecNames() const
{
	std::vector<std::string> names;
	names.reserve(_specs.size());
	for (const auto& pair : _specs)
		names.push_back(pair.first);
	return names;
}

const RpcMethodSpec* JaspRpcDispatcher::getSpec(const std::string& method) const
{
	auto it = _specs.find(method);
	return (it != _specs.end()) ? &it->second : nullptr;
}

std::string JaspRpcDispatcher::toolDisplayName(const std::string& method) const
{
	const RpcMethodSpec* spec = getSpec(method);
	if (!spec || spec->displayName.empty())
		return method;
	return spec->displayName;
}

// =========================================================================
//  Unregistration / introspection
// =========================================================================

void JaspRpcDispatcher::unregisterMethod(const std::string& method)
{
	_handlers.erase(method);
}

std::vector<std::string> JaspRpcDispatcher::registeredMethods() const
{
	std::vector<std::string> names;
	names.reserve(_handlers.size());
	for (const auto& pair : _handlers)
		names.push_back(pair.first);
	return names;
}

// =========================================================================
//  Built-in methods
// =========================================================================

void JaspRpcDispatcher::registerBuiltins()
{
	// ping — connectivity / liveness check
	registerMethod("ping", [](const Json::Value& params) -> Json::Value
	{
		Json::Value result;
		result["message"] = "pong";
		return result;
	});

	// rpc_discover — schema introspection
	registerMethod("rpc_discover", [this](const Json::Value&) -> Json::Value
	{
		Json::Value methods(Json::arrayValue);
		for (const auto& name : registeredMethods())
		{
			if (auto* spec = getSpec(name))
				methods.append(spec->toJson());
			else
			{
				// Methods without a spec still get a minimal object so
				// the response array is uniformly typed.
				Json::Value obj;
				obj["name"] = name;
				methods.append(obj);
			}
		}
		Json::Value result;
		result["methods"] = methods;
		return result;
	});
}

// =========================================================================
//  JSON-RPC 2.0 protocol helpers
// =========================================================================

Json::Value JaspRpcDispatcher::makeError(int code,
									 const std::string& message,
									 const Json::Value& id)
{
	Json::Value err;
	err["jsonrpc"] = "2.0";
	err["id"]      = id;
	err["error"]["code"]    = code;
	err["error"]["message"] = message;
	return err;
}

Json::Value JaspRpcDispatcher::makeError(int code,
									 const std::string& message,
									 const Json::Value& data,
									 const Json::Value& id)
{
	Json::Value err;
	err["jsonrpc"] = "2.0";
	err["id"]      = id;
	err["error"]["code"]    = code;
	err["error"]["message"] = message;
	err["error"]["data"]    = data;
	return err;
}

Json::Value JaspRpcDispatcher::makeResponse(const Json::Value& result,
											const Json::Value& id)
{
	Json::Value resp;
	resp["jsonrpc"] = "2.0";
	resp["id"]      = id;

	if (result.isMember("code") && result.isMember("message"))
	{
		// Looks like an error object from a handler — forward as error.
		resp["error"] = result;
	}
	else
	{
		resp["result"] = result;
	}

	return resp;
}

// =========================================================================
//  Dispatch
// =========================================================================

Json::Value JaspRpcDispatcher::dispatch(const Json::Value& request)
{
	Json::Value id = request.get("id", Json::nullValue);

	// Re-entrancy guard: a previous dispatch is still on the call stack
	// (likely waiting in a nested QEventLoop via waitAndProcessEvents).
	// Reject immediately to prevent state corruption in callers like
	// AiBridge and the HTTP server.
	if (m_inFlight)
		return makeError(-32000, "Procedure call already in flight", id);

	// JSON-RPC 2.0: "method" is required.
	if (!request.isMember("method") || !request["method"].isString())
		return makeError(-32600, "Invalid Request: missing 'method'", id);

	std::string method = request["method"].asString();
	auto it = _handlers.find(method);
	if (it == _handlers.end())
		return makeError(-32601, "Method not found: '" + method + "'", id);

	// Look up the spec to check x-failOnStateDiverged.
	bool failOnDiverged = false;
	auto specIt = _specs.find(method);
	if (specIt != _specs.end())
		failOnDiverged = specIt->second.failOnStateDiverged;

	Json::Value params = request.get("params", Json::objectValue);

	// --- Mutation methods: fail if workspace state diverged -------------
	// Only USER-INDUCED changes (options, data, add/remove) block mutations.
	// Background evolution (status transitions, results completing) does not —
	// those are reported via _stateUpdate on read calls.
	if (failOnDiverged)
	{
		if (auto * t = AgentStateTracker::tracker(); t && t->isUserDiverged())
		{
			Json::Value snapshot = t->buildWorkspaceSnapshot();
			t->markClean();
			return makeError(-32001,
				"Workspace state diverged since your last observation. "
				"Current state included in data. Review it and retry your call.",
				snapshot, id);
		}
	}

	m_inFlight = true;
	try
	{
		Json::Value result = it->second(params);
		m_inFlight = false;

		// If the handler returned an error, wrap it properly.
		if (result.isObject() && result.isMember("code") &&
			result.isMember("message"))
		{
			return makeError(result["code"].asInt(),
							 result["message"].asString(), id);
		}

		// --- Read methods: attach _stateUpdate if workspace is dirty ------
		// The snapshot piggybacks on the normal result.  After delivery the
		// baseline advances so the snapshot only appears when something
		// actually changed.
		//
		// Data dirty flags are cleared unconditionally on every read — the
		// agent is observing the workspace, so data changes are implicitly
		// seen.  This prevents a stale _dataState.dirty from triggering an
		// unnecessary _stateUpdate or a spurious divergence on the next call.
		if (!failOnDiverged)
		{
			AgentStateTracker::notifyDataObserved();

			if (auto * t = AgentStateTracker::tracker(); t && t->isDirty())
			{
				result["_stateUpdate"] = t->buildWorkspaceSnapshot();
				t->markClean();
			}
		}
		else
		{
			// --- Mutation methods: clear dirty flags from our own actions ---
			// A successful mutation means the workspace was clean when it
			// started.  Any dirty flags now set were caused by the mutation
			// itself (analysisAdded signal from creating a report, status
			// changes from analysis_run, etc.).  Clear them so the next
			// mutation isn't blocked by its own predecessor's side effects.
			if (auto * t = AgentStateTracker::tracker())
				t->markClean();
		}

		return makeResponse(result, id);
	}
	catch (const std::exception& e)
	{
		m_inFlight = false;
		return makeError(-32603, std::string("Internal error: ") + e.what(), id);
	}
}

std::string JaspRpcDispatcher::dispatch(const std::string& requestJson)
{
	Json::Value  req;
	Json::Reader reader;
	if (!reader.parse(requestJson, req))
	{
		// Build a parse-error response manually.
		Json::Value err;
		err["jsonrpc"] = "2.0";
		err["id"]      = Json::nullValue;
		err["error"]["code"]    = -32700;
		err["error"]["message"] = "Parse error";
		Json::StreamWriterBuilder builder;
		builder["indentation"] = "";
		return Json::writeString(builder, err);
	}

	Json::Value resp = dispatch(req);

	Json::StreamWriterBuilder builder;
	builder["indentation"] = "";
	return Json::writeString(builder, resp);
}