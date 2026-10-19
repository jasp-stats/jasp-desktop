//
// JaspRpcServer - HTTP transport for JASP-RPC using QtHttpServer.
//
// Binds a QHttpServer to a QTcpServer, exposes a single POST /rpc endpoint.
// Incoming JSON-RPC bodies are forwarded to JaspRpcDispatcher and the result
// is sent back as HTTP 200 with Content-Type: application/json.
//

#ifndef JASPRPCSERVER_H
#define JASPRPCSERVER_H

#include <QObject>
#include <QHttpServer>
#include <QTcpServer>
#include <memory>
#include <string>

class JaspRpcDispatcher;

class JaspRpcServer : public QObject
{
	Q_OBJECT

public:
	/// Construct a server.
	/// The dispatcher must outlive the server.
	explicit JaspRpcServer(JaspRpcDispatcher& dispatcher,
                           QObject* parent = nullptr,
	                       const QString& host = "127.0.0.1",
                           quint16 port = 48164,
                           const QString& endpointPath = "/rpc");

	~JaspRpcServer() override;

	/// Start listening. Returns true on success.
	bool start();

	/// Stop listening.
	void stop();

	/// Return the actual port (useful when port=0 was passed).
	quint16 serverPort() const;

private:
	JaspRpcDispatcher& _dispatcher;
	QString             _host;
	quint16             _port;
	QString             _endpointPath;
	QHttpServer         _httpServer;
	QTcpServer*         _tcpServer = nullptr;
};

#endif // JASPRPCSERVER_H
