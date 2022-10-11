#include "tabdirectionfilter.h"
#include "log.h"
#include <QtGui/qevent.h>

TabDirectionFilter::Direction TabDirectionFilter::tabDirectionForward = Forward;

TabDirectionFilter::TabDirectionFilter(QObject *parent)
	: QObject{parent}
{

}


bool TabDirectionFilter::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::KeyPress)
	{
		QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
		if (keyEvent->key() == Qt::Key_Tab)
			tabDirectionForward = Forward;
		else if (keyEvent->key() == Qt::Key_Backtab)
			tabDirectionForward = Backward;
		Log::log() << "filter hit: " << (tabDirectionForward == Backward ? "backward" : "forward") << std::endl;
	}
	return false;
}
