#include "webview.h"
#include <QWebFrame>

WebView::WebView(QWidget *parent):
	QWebView(parent)
{

}

void WebView::paintEvent(QPaintEvent * event)
{
	QWebView::paintEvent(event);

	QWebFrame *frameff = page()->mainFrame();

	int hValue = frameff->scrollBarValue(Qt::Horizontal);
	int vValue = frameff->scrollBarValue(Qt::Vertical);

	if (hValue != _lastHorizontalValue || vValue != _lastVerticalValue)
	{
		_lastHorizontalValue = hValue;
		_lastVerticalValue = vValue;

		emit scrollValueChanged();
	}

}
