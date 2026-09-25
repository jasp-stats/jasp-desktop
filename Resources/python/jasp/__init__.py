"""Call JASP from Python.

Every method JASP offers over JSON-RPC, the same ones its AI uses (see
Resources/JASP_RPC.json), is a function of this module:

    import jasp

    jasp.data_load(path="/data/survey.csv", wait=True)
    analysis = jasp.analysis_create("jaspDescriptives", "Descriptives")
    jasp.analysis_run(analysis["analysisId"], {"variables": ["age"]}, wait=True)

help(jasp.analysis_run) describes a method and its parameters.

A script that JASP runs is connected as soon as it imports this module:
JASP hands it JASP_RPC_URL and JASP_RPC_TOKEN. Anywhere else, call
connect() with the url and token of JASP's RPC server first.

Only the standard library is used, so any Python 3.8 or later will do.
"""

import inspect
import json
import keyword
import os
import urllib.error
import urllib.request

__all__ = ["RpcError", "call", "connect", "connected"]  # connect() adds JASP's methods


class RpcError(Exception):
    """A call JASP refused, or that failed in JASP.

    code is the JSON-RPC error code, or None when the method itself reported
    the failure; data holds whatever details JASP sent along."""

    def __init__(self, message, code=None, data=None):
        super().__init__(message)
        self.code = code
        self.data = data


class _Unset:
    """The default of an optional parameter that JASP fills in itself."""

    def __repr__(self):
        return "<JASP's default>"


_UNSET   = _Unset()
_opener  = urllib.request.build_opener(urllib.request.ProxyHandler({}))  # JASP runs on this computer: never through a proxy
_url     = None
_token   = None
_lastId  = 0
_methods = []  # The functions connect() made


def connected():
    """Whether this module is connected to JASP."""
    return _url is not None


def connect(url=None, token=None):
    """Connects to JASP and makes a function of every method it offers.

    Without arguments it takes JASP_RPC_URL and JASP_RPC_TOKEN from the
    environment, which JASP sets for a script it runs."""
    global _url, _token

    url   = url or os.environ.get("JASP_RPC_URL")
    token = token if token is not None else os.environ.get("JASP_RPC_TOKEN")

    if not url:
        raise ConnectionError("There is no JASP to connect to: run this script from JASP, "
                              "or give connect() the url and token of JASP's RPC server")

    _url, _token = url, token

    try:
        specs = _call("rpc_discover", {})["methods"]
    except BaseException:
        _url = _token = None
        raise

    for name in _methods:
        globals().pop(name, None)
        __all__.remove(name)
    _methods.clear()

    for spec in specs:
        name = spec.get("name", "")

        # A method whose name Python cannot use, or that this module uses itself, stays reachable through call()
        if name.isidentifier() and not keyword.iskeyword(name) and name not in globals():
            globals()[name] = _function(spec)
            _methods.append(name)
            __all__.append(name)  # help(jasp) and "from jasp import *" only see what __all__ names


def call(method, /, **params):
    """Calls a JASP method by its name, with its parameters as keywords, and returns its result."""
    return _call(method, params)


def _call(method, params):
    global _lastId

    if _url is None:
        raise ConnectionError("Not connected to JASP: call jasp.connect() first")

    _lastId += 1
    body    = json.dumps({"jsonrpc": "2.0", "id": _lastId, "method": method, "params": params}).encode("utf-8")
    headers = {"Content-Type": "application/json"}

    if _token:
        headers["Authorization"] = "Bearer " + _token

    try:
        # No timeout: a call that waits for R, or for its turn behind another call, may take a while
        with _opener.open(urllib.request.Request(_url, data=body, headers=headers, method="POST")) as response:
            reply = json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        if error.code == 401:
            raise PermissionError("JASP refused the token for " + _url) from None
        raise ConnectionError("JASP answered %s with HTTP status %d" % (_url, error.code)) from None
    except urllib.error.URLError as error:
        raise ConnectionError("Cannot reach JASP at %s: %s" % (_url, error.reason)) from None

    if "error" in reply:
        error = reply["error"]
        raise RpcError(error.get("message", "Unknown error"), error.get("code"), error.get("data"))

    result = reply.get("result")

    # A method reporting its own failure answers {"status": "error", "message": ...}
    if isinstance(result, dict) and result.get("status") == "error":
        raise RpcError(result.get("message", "Unknown error"), None, result)

    return result


def _function(spec):
    """A Python function calling a JASP method, with the parameters, defaults and help its spec describes."""
    name   = spec["name"]
    params = spec.get("params", [])

    parameters, keywordOnly = [], False

    for param in params:
        paramName = param.get("name", "")

        if not paramName.isidentifier() or keyword.iskeyword(paramName):
            parameters = None  # Not a name Python can use: the function then takes keywords only
            break

        required = param.get("required", True)

        # A required parameter after an optional one can only be given by name, and so can all after it
        if required and parameters and parameters[-1].default is not inspect.Parameter.empty:
            keywordOnly = True

        parameters.append(inspect.Parameter(
            paramName,
            inspect.Parameter.KEYWORD_ONLY if keywordOnly else inspect.Parameter.POSITIONAL_OR_KEYWORD,
            default=inspect.Parameter.empty if required else param.get("schema", {}).get("default", _UNSET)))

    signature = inspect.Signature(parameters) if parameters is not None else None

    def method(*args, **kwargs):
        if signature is None:
            if args:
                raise TypeError(name + "() takes its parameters by name only")
            return _call(name, kwargs)

        # Like any Python function: a TypeError for a missing or unknown parameter.
        # Only what was given is sent, JASP fills in the defaults itself.
        return _call(name, signature.bind(*args, **kwargs).arguments)

    lines = [spec.get("summary") or name]

    if params:
        lines += ["", "Parameters:"]
        for param in params:
            schema = param.get("schema", {})
            kind   = schema.get("type", "any")

            if not param.get("required", True):
                kind += ", optional" + (", default " + json.dumps(schema["default"]) if "default" in schema else "")

            lines.append("    %s (%s)" % (param.get("name", ""), kind))

            if param.get("description"):
                lines.append("        " + param["description"])

    method.__name__     = method.__qualname__ = name
    method.__module__   = __name__
    method.__doc__      = "\n".join(lines)

    if signature is not None:
        method.__signature__ = signature

    return method


if os.environ.get("JASP_RPC_URL"):
    connect()
