//
// JaspRpcServer - HTTP transport for JASP-RPC using QtHttpServer.
//
// Binds a QHttpServer to a QTcpServer, exposes a single POST /rpc endpoint.
// Incoming JSON-RPC bodies are forwarded to JaspRpcDispatcher and the result
// is sent back as HTTP 200 with Content-Type: application/json.
//
// A server may require a token: then every request must carry
// "Authorization: Bearer <token>", or it is answered with 401 and never
// reaches the dispatcher.  startForScript() makes such a server for one run
// of a script, on a free port and with a new token.
//

#ifndef JASPRPCSERVER_H
#define JASPRPCSERVER_H

#include <QObject>
#include <QHttpServer>
#include <QHttpServerResponder>
#include <QTcpServer>
#include <memory>
#include <string>
#include <unordered_map>

#include "jasprpcdispatcher.h"

class JaspRpcServer : public QObject
{
	Q_OBJECT

public:
	/// Construct a server.
	/// The dispatcher must outlive the server.
	/// @param caller  who the calls through this server count as (see RpcCaller)
	/// @param token   when not empty, the token every request must carry
	explicit JaspRpcServer(JaspRpcDispatcher& dispatcher,
                           QObject* parent = nullptr,
	                       const QString& host = "127.0.0.1",
                           quint16 port = 48164,
                           const QString& endpointPath = "/rpc",
	                       RpcCaller caller = RpcCaller::Ai,
	                       const QString& token = "");

	~JaspRpcServer() override;

	/// A server for one run of a script: its calls count as RpcCaller::Script,
	/// it listens on a free port of 127.0.0.1 only, and it answers only requests
	/// carrying its own new token.  Deleting it closes it again, so it only lives
	/// as long as the run.  Returns nullptr when it cannot listen.
	static std::unique_ptr<JaspRpcServer> startForScript(JaspRpcDispatcher& dispatcher, QObject* parent = nullptr);

	/// A new random token: 32 bytes from the system's secure random source, as hex.
	static QString newToken();

	/// Start listening. Returns true on success.
	bool start();

	/// Stop listening.
	void stop();

	/// Return the actual port (useful when port=0 was passed).
	quint16 serverPort() const;

	/// Where to send requests, e.g. http://127.0.0.1:52817/rpc
	QString url() const;

	/// The token requests must carry, empty when none is needed.
	const QString& token() const { return _token; }

private:
	/// Answers a request whose call has run (see JaspRpcDispatcher::dispatchWhenFree).
	void respond(quint64 requestId, const std::string& output);

	/// Whether a request's Authorization header carries this server's token.
	bool authorized(const QHttpServerRequest& request) const;

	JaspRpcDispatcher& _dispatcher;
	QString             _host;
	quint16             _port;
	QString             _endpointPath;
	RpcCaller           _caller;
	QString             _token;
	QHttpServer         _httpServer;
	QTcpServer*         _tcpServer = nullptr;

	/// Requests whose call waits for its turn, by request id.
	std::unordered_map<quint64, QHttpServerResponder> _waiting;
	quint64             _nextRequestId = 0;
};

#endif // JASPRPCSERVER_H
