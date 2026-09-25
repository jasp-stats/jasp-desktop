//
// JaspRpcServer implementation — see jasprpcserver.h for API docs.
//
// Wraps QHttpServer (QtHttpServer module) on top of QTcpServer.
// Exposes a single POST /rpc endpoint.
// Incoming JSON-RPC bodies are forwarded to JaspRpcDispatcher and
// the result is returned as HTTP 200 with Content-Type: application/json.
//

#include "jasprpcserver.h"
#include "jasprpcdispatcher.h"

#include <QHttpServerRequest>
#include <QHttpServerResponse>
#include <QHostAddress>
#include <QPointer>
#include "log.h"

JaspRpcServer::JaspRpcServer(JaspRpcDispatcher& dispatcher,
                              QObject* parent,
							 const QString& host,
							 quint16 port,
                              const QString& endpointPath)
	: QObject(parent)
	, _dispatcher(dispatcher)
	, _host(host)
	, _port(port)
	, _endpointPath(endpointPath)
{
}

JaspRpcServer::~JaspRpcServer()
{
	stop();
}

bool JaspRpcServer::start()
{
	// ---- POST /rpc — main JSON-RPC endpoint ----
	// A call arriving while another is in flight (waiting for R in a nested
	// event loop) waits its turn rather than being refused: the responder is
	// kept, and the client gets its answer once its call has run.
	_httpServer.route(_endpointPath,
					  QHttpServerRequest::Method::Post,
					  [this](const QHttpServerRequest& request, QHttpServerResponder&& responder)
	{
		const QByteArray body    = request.body();
		const std::string input(body.constData(), body.size());

		const quint64 requestId = _nextRequestId++;
		_waiting.emplace(requestId, std::move(responder));

		_dispatcher.dispatchWhenFree(input, RpcCaller::Ai,
			[server = QPointer<JaspRpcServer>(this), requestId](const std::string& output)
			{
				if (server) // Gone when JASP closed before this call's turn came
					server->respond(requestId, output);
			});
	});

	// ---- OPTIONS /rpc — CORS pre-flight for browser-based clients ----
	_httpServer.route(_endpointPath,
					  QHttpServerRequest::Method::Options,
					  [](const QHttpServerRequest&)
	{

		QHttpHeaders corsHeaders;
		corsHeaders.append("Access-Control-Allow-Origin",  "*");
		corsHeaders.append("Access-Control-Allow-Methods", "POST, OPTIONS");
		corsHeaders.append("Access-Control-Allow-Headers", "Content-Type");

		QHttpServerResponse resp(QHttpServerResponse::StatusCode::Ok);
		resp.setHeaders(corsHeaders);


		return resp;
	});

	// ---- TCP listener ----
	_tcpServer = new QTcpServer(this);

	if (!_tcpServer->listen(QHostAddress(_host), _port))
	{
		Log::log() << "[JaspRpcServer] Failed to listen on "
				  << _host.toStdString() << ":" << _port
				  << " — " << _tcpServer->errorString().toStdString() << std::endl;
		return false;
	}

	if (!_httpServer.bind(_tcpServer))
	{
		Log::log() << "[JaspRpcServer] Failed to bind QHttpServer to TCP socket"
				  << std::endl;
		return false;
	}

	Log::log() << "[JaspRpcServer] Listening on http://"
			  << _host.toStdString() << ":" << _tcpServer->serverPort()
			  << _endpointPath.toStdString() << std::endl;

	return true;
}

void JaspRpcServer::stop()
{
	if (_tcpServer)
	{
		_tcpServer->close();
		// _tcpServer is parented to this, so it will be deleted with us
	}

	Log::log() << "[JaspRpcServer] Stopped." << std::endl;
}

quint16 JaspRpcServer::serverPort() const
{
	return _tcpServer ? _tcpServer->serverPort() : 0;
}

void JaspRpcServer::respond(quint64 requestId, const std::string& output)
{
	auto it = _waiting.find(requestId);
	if (it == _waiting.end())
		return;

	if (!it->second.isResponseCanceled()) // The client may have given up waiting
		it->second.write(QByteArray::fromStdString(output), "application/json");

	_waiting.erase(it);
}
