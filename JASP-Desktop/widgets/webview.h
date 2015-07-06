#ifndef WEBVIEW_H
#define WEBVIEW_H

#include <QWebView>

#include "common.h"

class WebView : public QWebView
{

	Q_OBJECT

public:
	explicit WebView(QWidget *parent = 0);

signals:
	void scrollValueChanged();

protected:
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private:
	int _lastVerticalValue;
	int _lastHorizontalValue;
};

#endif // WEBVIEW_H
