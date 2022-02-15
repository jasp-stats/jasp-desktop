#ifndef PLOTSCHEMEHANDLER_H
#define PLOTSCHEMEHANDLER_H

#include "tempfiles.h"
#include <QWebEngineUrlRequestJob>
#include <QWebEngineUrlScheme>
#include <QWebEngineUrlSchemeHandler>
#include <QQuickWebEngineProfile>
#include <QFile>


///This has been added because webengine doesnt allow us loading from "file://...." anymore since Qt6.
///
/// So now, because adding arguments like --disable-websecurity and stuff did not help this custom scheme handler has been added to load png-files (and anything really, but this is what we need)
/// It also doesn't help to define a QWebEngineUrlScheme with "LocalScheme | LocalAccessAllowed" because LocalAccessAllowed is ignored entirely and js will just not load it.
/// Because it couldn't be set to "Local"  also "/C:/..."  is also converted into "/c/..." which means it wouldn't be loadable,
/// so now just a relative path is given and the plotschemehandler just looks in tempdir for it.
class PlotSchemeHandler : public QWebEngineUrlSchemeHandler
{
public:
	PlotSchemeHandler(QObject * parent = nullptr);


	static void createUrlScheme();

	void requestStarted(QWebEngineUrlRequestJob *request) override;
};

#endif // PLOTSCHEMEHANDLER_H
