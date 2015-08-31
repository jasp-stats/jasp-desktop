
#include "elidelabel.h"

ElideLabel::ElideLabel(QWidget *parent) : QLabel(parent)
{
	setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
}

QSize ElideLabel::minimumSizeHint() const
{
	return QSize(10, QLabel::minimumSizeHint().height());
}

QSize ElideLabel::sizeHint() const
{
	return QSize(10, QLabel::sizeHint().height());
}

void ElideLabel::resizeEvent(QResizeEvent *event)
{
	if (text() != _modifiedText)
		_originalText = text();

	QFontMetrics metrics(font());

	_modifiedText = metrics.elidedText(_originalText, Qt::ElideRight, width());

	setText(_modifiedText);

	QLabel::resizeEvent(event);
}

