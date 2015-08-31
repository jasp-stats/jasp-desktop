
#include "fsbrowser.h"

#include <QGridLayout>
#include <QScrollArea>

#include "verticalscrollarea.h"
#include "fsentrywidget.h"

FSBrowser::FSBrowser(QWidget *parent) : QWidget(parent)
{
	_browseMode = FSBrowser::BrowseOpenFile;
	_viewType = FSBrowser::IconView;

	QGridLayout *layout = new QGridLayout(this);
	layout->setContentsMargins(0, 0, 0, 0);
	setLayout(layout);

	VerticalScrollArea *scrollArea = new VerticalScrollArea(this);
	scrollArea->setFrameShape(QScrollArea::NoFrame);
	layout->addWidget(scrollArea);

	_scrollPane = new QWidget;
	scrollArea->setWidget(_scrollPane);

	_scrollPaneLayout = new QVBoxLayout(_scrollPane);
	_scrollPaneLayout->setSpacing(1);
	_scrollPaneLayout->setSizeConstraint(QLayout::SetMinAndMaxSize);
	_scrollPane->setLayout(_scrollPaneLayout);

	_buttonGroup = new QButtonGroup(this);
}

void FSBrowser::setFSModel(FSBModel *model)
{
	_model = model;
	_model->refresh();
	refresh();

	connect(_model, SIGNAL(entriesChanged()), this, SLOT(refresh()));
}

void FSBrowser::setBrowseMode(FSBrowser::BrowseMode mode)
{
	_browseMode = mode;
}

void FSBrowser::setViewType(FSBrowser::ViewType viewType)
{
	_viewType = viewType;
}

void FSBrowser::refresh()
{
	bool compact = false;

	if (_viewType == ListView)
	{
		compact = true;
		_scrollPaneLayout->setContentsMargins(8, 8, 8, 8);
		_scrollPaneLayout->setSpacing(0);
	}
	else
	{
		_scrollPaneLayout->setContentsMargins(12, 12, 12, 12);
		_scrollPaneLayout->setSpacing(8);
	}

	foreach (QAbstractButton *button, _buttonGroup->buttons())
		delete button;

	int id = 0;

	foreach (const FSEntry &entry, _model->entries())
	{
		FSEntryWidget *button = new FSEntryWidget(entry, _scrollPane);
		button->setCompact(compact);

		_buttonGroup->addButton(button, id++);
		_scrollPaneLayout->addWidget(button);

		connect(button, SIGNAL(opened()), this, SLOT(entryOpenedHandler()));
	}
}

void FSBrowser::entryOpenedHandler()
{
	FSEntryWidget *entry = qobject_cast<FSEntryWidget*>(this->sender());

	if (_browseMode == BrowseOpenFolder)
	{
		emit entryOpened(entry->path());
	}
	else
	{
		if (entry->entryType() == FSEntry::Folder)
			_model->setPath(entry->path());
		else
			emit entryOpened(entry->path());
	}
}

