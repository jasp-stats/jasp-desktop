
#include "boundtextedit.h"

#include "utils.h"

BoundTextEdit::BoundTextEdit(QWidget *parent) :
	QPlainTextEdit(parent)
{
	_boundTo = NULL;

	this->setLineWrapMode(QPlainTextEdit::NoWrap);

	QFont font("Monospace");
	font.setStyleHint(QFont::Monospace);
	this->setFont(font);

	QFontMetrics metrics(font);
	this->setTabStopWidth(metrics.width("    ") + 2);
}

void BoundTextEdit::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);

	if (_boundTo != NULL)
	{
		QString text = tq(_boundTo->value());
		setPlainText(text);
	}

}

void BoundTextEdit::keyPressEvent(QKeyEvent *event)
{
	if (_boundTo != NULL)
	{
		int modifiers = Qt::ControlModifier | Qt::MetaModifier;
		if ((event->modifiers() & modifiers) && event->key() == Qt::Key_Return)
			_boundTo->setValue(fq(toPlainText()));
		else
			QPlainTextEdit::keyPressEvent(event);
	}
	else
	{
		QPlainTextEdit::keyPressEvent(event);
	}
}
