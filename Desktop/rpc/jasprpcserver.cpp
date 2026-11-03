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
#include <QJsonObject>
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
	_httpServer.route(_endpointPath,
					  QHttpServerRequest::Method::Post,
					  [this](const QHttpServerRequest& request)
	{
		const QString rejected = rejectionReason(request);
		if (!rejected.isEmpty())
		{
			Log::log() << "[JaspRpcServer] Refused a request: " << rejected.toStdString() << std::endl;
			const QJsonObject error {{"code", -32600}, {"message", "JASP refused the request: " + rejected}};
			return QHttpServerResponse(QJsonObject{{"jsonrpc", "2.0"}, {"id", QJsonValue::Null}, {"error", error}},
									   QHttpServerResponse::StatusCode::Forbidden);
		}

		const QByteArray body    = request.body();
		const std::string input(body.constData(), body.size());

		const std::string output = _dispatcher.dispatch(input);

		return QHttpServerResponse(
			QByteArray::fromStdString(output),
			QHttpServerResponse::StatusCode::Ok);
	});

	// No OPTIONS route and no CORS headers: browser pages must not be able to reach this endpoint.

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

QString JaspRpcServer::rejectionReason(const QHttpServerRequest& request) const
{
	const QHttpHeaders & headers = request.headers();

	// Browsers send Origin with every cross-site POST (even "no-cors" ones); local tools such as jasp-mcp do not.
	if (headers.contains(QHttpHeaders::WellKnownHeader::Origin))
		return "requests from web pages (with an Origin header) are not accepted";

	// value() only returns the first of repeated fields, so a second Host or Content-Type would go unchecked.
	if (headers.values(QHttpHeaders::WellKnownHeader::Host).size() != 1 || headers.values(QHttpHeaders::WellKnownHeader::ContentType).size() != 1)
		return "exactly one Host and one Content-Type header are required";

	// A DNS-rebinding page reaches 127.0.0.1 under its own host name, so only accept the names of this listener.
	const QString	host			= QString::fromLatin1(headers.value(QHttpHeaders::WellKnownHeader::Host)).toLower(),
					listenHost		= _host.contains(':') ? "[" + _host.toLower() + "]" : _host.toLower(), // IPv6 literal
					port			= ":" + QString::number(serverPort());
	QStringList acceptedHosts;
	for (const QString & name : { QString("127.0.0.1"), QString("localhost"), QString("[::1]"), listenHost })
		acceptedHosts << name + port << (serverPort() == 80 ? name : QString()); // clients omit the default port
	if (host.isEmpty() || !acceptedHosts.contains(host))
		return "unexpected Host header '" + host.left(100) + "'";

	// Pages can send text/plain without a CORS pre-flight, application/json they cannot.
	const QByteArray mediaType = QByteArray(headers.value(QHttpHeaders::WellKnownHeader::ContentType)).split(';').first().trimmed().toLower();
	if (mediaType != "application/json")
		return "Content-Type must be application/json";

	return {};
}
