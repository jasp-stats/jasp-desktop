//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

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
