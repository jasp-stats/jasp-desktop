
#include "backstagepage.h"

BackstagePage::BackstagePage(QWidget *parent) : QWidget(parent)
{
	_mode = FileEvent::FileOpen;
}

void BackstagePage::setMode(FileEvent::FileMode mode)
{
	_mode = mode;
}

