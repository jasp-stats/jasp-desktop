//
// JaspRpcDispatcher - Central JSON-RPC 2.0 dispatch hub for JASP.
//
// Maps incoming method strings to C++ handlers using a registry pattern.
// Handlers are std::function<Json::Value(const Json::Value& params)>.
// dispatch() is safe to call from the Qt event loop; handlers run
// synchronously so must not block for long periods.
//
// In addition to the low-level registerMethod(string, handler) the
// dispatcher accepts OpenRPC-inspired method specs that describe
// parameter / result schemas.  When a spec is provided the dispatcher
// automatically validates incoming params against the schema, fills in
// any declared default values, and validates the handler's return value
// before sending it to the client.
//
// Schema types (RpcSchema, RpcParamSpec, RpcResultSpec, RpcMethodSpec)
// live in rpcschema.h — keeping the dispatcher header focused on
// registration and dispatch logic.
//

#ifndef JASPRPCDISPATCHER_H
#define JASPRPCDISPATCHER_H

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

#include <QEventLoop>
#include <QTimer>

#include "json/json.h"
#include "rpcschema.h"

// =========================================================================
//  Handler type
// =========================================================================

using RpcHandler = std::function<Json::Value(const Json::Value& params)>;

// =========================================================================
//  Dispatcher
// =========================================================================

class JaspRpcDispatcher
{
public:
	JaspRpcDispatcher();
	~JaspRpcDispatcher();

	static JaspRpcDispatcher* singleton() { return _singleton; }

	// ------------------------------------------------------------------
	// Registration — low level (no automatic validation)
	// ------------------------------------------------------------------

	/// Register a bare handler.  No param/result validation is performed.
	/// Returns false if the method name is already taken.
	bool registerMethod(const std::string& method, RpcHandler handler);

	// ------------------------------------------------------------------
	// Registration — flat param spec (backward-compatible)
	// ------------------------------------------------------------------

	/// Register with a flat (non-nested) list of parameter descriptors.
	/// Missing required params → error; missing optional → filled with null.
	bool registerMethod(const std::string& method,
						std::vector<RpcParamSpec> paramSpec,
						RpcHandler handler);

	// ------------------------------------------------------------------
	// Registration — full OpenRPC method spec
	// ------------------------------------------------------------------

	/// Register together with an OpenRPC-style spec.  The wrapper:
	///   1. Validates incoming `params` against the declared schemas.
	///   2. Applies declared `default` values for missing optional params.
	///   3. Calls the handler with the validated, defaulted params.
	///   4. Validates the handler's return value against `result.schema`.
	/// Returns false if the method name is already registered.
	bool registerMethod(const RpcMethodSpec& spec, RpcHandler handler);

	/// Convenience: parse `specJson` then call registerMethod(RpcMethodSpec, RpcHandler).
	/// Example:
	///   disp->registerMethodFromSpec(R"({"name":"foo","params":[...],"result":{...}})", handler);
	bool registerMethodFromSpec(const std::string& specJson, RpcHandler handler);

	// ------------------------------------------------------------------
	// Registration — by name (from the pre-loaded spec registry)
	// ------------------------------------------------------------------

	/// Look up `methodName` in the internal spec registry (populated by
	/// loadSpecFile / loadSpecFromString), wrap the handler with the same
	/// 4-step pipeline as registerMethod(RpcMethodSpec, handler).
	///
	/// Returns false if the name is not found in the spec registry or the
	/// handler slot is already taken.
	bool registerMethodByName(const std::string& methodName, RpcHandler handler);

	// ------------------------------------------------------------------
	// Spec-file loading
	// ------------------------------------------------------------------

	/// Load an OpenRPC 1.x document from a JSON string.
	/// Every entry in the "methods" array is parsed into the internal spec
	/// registry so that registerMethodByName() can find it later.
	/// Returns the number of methods loaded, or -1 on parse error.
	int loadSpecFromString(const std::string& openRpcJson);

	/// Load an OpenRPC 1.x document from a file on disk.
	/// Reads the file, then delegates to loadSpecFromString.
	int loadSpecFile(const std::string& path);

	/// Return the method names currently in the spec registry.
	std::vector<std::string> knownSpecNames() const;

	// ------------------------------------------------------------------
	// Unregistration / introspection
	// ------------------------------------------------------------------

	void unregisterMethod(const std::string& method);
	std::vector<std::string> registeredMethods() const;

	/// Return the spec for a given method name, or nullptr if not found.
	const RpcMethodSpec* getSpec(const std::string& method) const;

	/// Return the human-readable display name for a method, or the method name itself if none set.
	std::string toolDisplayName(const std::string& method) const;

	// ------------------------------------------------------------------
	// Dispatch
	// ------------------------------------------------------------------

	std::string dispatch(const std::string& requestJson);
	Json::Value dispatch(const Json::Value& request);

	// ------------------------------------------------------------------
	// Re-entrancy
	// ------------------------------------------------------------------

	/// True while a dispatch() is on the call stack — a handler is
	/// executing and has not yet returned.  Callers (AiBridge, the HTTP
	/// server, etc.) can check this to avoid kicking off concurrent
	/// operations that would corrupt shared state.
	bool inFlight() const { return m_inFlight; }

	// ------------------------------------------------------------------
	// Nested event loop helper — safe blocking wait for RPC handlers
	// ------------------------------------------------------------------

	/// Run a nested QEventLoop for up to @p timeoutMs milliseconds.
	///
	/// Unlike QThread::sleep() or std::condition_variable::wait(),
	/// QEventLoop::exec() continues to process Qt events (paints,
	/// signals, timers) so the UI stays responsive and cross-thread
	/// signals (e.g. R engine results) are delivered normally.
	///
	/// The @p setup callback receives references to the loop and a
	/// single-shot timer.  Connect your condition signals to
	/// `loop.quit()` — the timer timeout is already wired to
	/// `loop.quit()`, so the loop will never block indefinitely.
	///
	/// After this returns, check your own state to determine whether
	/// the condition was met or the timeout fired.
	///
	/// Example:
	/// @code
	/// bool finished = false;
	/// JaspRpcDispatcher::waitAndProcessEvents(timeoutMs,
	///   [&](QEventLoop& loop, QTimer& timer) {
	///     QObject::connect(obj, &SomeClass::someSignal, &loop,
	///       [&]() { finished = true; loop.quit(); });
	///   });
	/// if (finished) { ... }
	/// @endcode
	static void waitAndProcessEvents(int timeoutMs,
		std::function<void(QEventLoop& loop, QTimer& timer)> setup);

	// ------------------------------------------------------------------
	// Static helpers — application-level payloads
	// ------------------------------------------------------------------

	static Json::Value successResult();                        // {"status":"success"}
	static Json::Value errorResult(const std::string& message); // {"status":"error","message":"..."}

	// ------------------------------------------------------------------
	// Static helpers — validation (callable from any handler)
	// ------------------------------------------------------------------

	/// Validate `params` against an array of parameter specs.
	/// Returns Json::nullValue on success, or an error object on failure.
	static Json::Value validateParams(const Json::Value& params,
									  const std::vector<RpcParamSpec>& spec);

	/// Validate a single value against a schema.
	static Json::Value validateSchema(const Json::Value& value,
									   const RpcSchema& schema);

	/// Validate a handler's return value against a result spec.
	static Json::Value validateResult(const Json::Value& result,
									  const RpcResultSpec& spec);

	/// Apply declared defaults from a param spec.  Does NOT check required
	/// fields — call validateParams() first.
	static Json::Value applyDefaults(const Json::Value& params,
									 const std::vector<RpcParamSpec>& spec);

private:
	/// Register built-in methods (ping, rpc_discover) that don't belong to
	/// any particular subsystem but are part of the JSON-RPC 2.0 core.
	void registerBuiltins();

	static Json::Value makeError(int code, const std::string& message,
							 const Json::Value& id);
	static Json::Value makeError(int code, const std::string& message,
							 const Json::Value& data,
							 const Json::Value& id);
	static Json::Value makeResponse(const Json::Value& result,
									const Json::Value& id);

	static JaspRpcDispatcher* _singleton;
	bool m_inFlight = false;
	std::unordered_map<std::string, RpcHandler> _handlers;

	/// Spec registry: method name -> parsed RpcMethodSpec.
	std::unordered_map<std::string, RpcMethodSpec> _specs;
};

#endif // JASPRPCDISPATCHER_H