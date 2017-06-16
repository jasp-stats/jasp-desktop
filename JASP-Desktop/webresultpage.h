#ifndef WEBRESULTPAGE_H
#define WEBRESULTPAGE_H

#include <QWebEnginePage>

class WebResultPage : public QWebEnginePage
{
    Q_OBJECT
public:
	explicit WebResultPage(QObject *parent = nullptr) : QWebEnginePage(parent) {}
protected:
    bool acceptNavigationRequest(const QUrl &url, NavigationType type, bool isMainFrame);
};

#endif // WEBRESULTPAGE_H
