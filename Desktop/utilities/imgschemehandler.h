#ifndef IMGSCHEMEHANDLER_H
#define IMGSCHEMEHANDLER_H

#include <QWebEngineUrlSchemeHandler>

///Like PlotSchemeHandler but for images in jaspHtml/results, https://github.com/jasp-stats/jasp-test-release/issues/2321
/// Supports svg and png only
class ImgSchemeHandler : public QWebEngineUrlSchemeHandler
{
  public:
	explicit ImgSchemeHandler(QObject *parent = nullptr);

	static void createUrlScheme();

	void requestStarted(QWebEngineUrlRequestJob *request) override;
};

#endif // IMGSCHEMEHANDLER_H
