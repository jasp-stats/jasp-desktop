#include "backstageexamples.h"

BackstageExamples::BackstageExamples(QWidget *parent) : BackstagePage(parent)
{
	
	QGridLayout *layout = new QGridLayout(this);
	layout->setSpacing(0);
	layout->setContentsMargins(0, 0, 0, 0);
	setLayout(layout);
			
	QWidget *topRow = new QWidget(this);
	layout->addWidget(topRow);
	
	QGridLayout *topRowLayout = new QGridLayout();
	topRowLayout->setContentsMargins(0, 6, 12, 0);
	topRow->setLayout(topRowLayout);

	QLabel *label = new QLabel("Examples", topRow);
	QFont f= QFont("SansSerif");
	f.setPointSize(18);
	label->setFont(f);
	QSizePolicy sp = label->sizePolicy();
	sp.setHorizontalStretch(1);
	label->setSizePolicy(sp);
	label->setContentsMargins(10, 6, 0, 0);
	topRowLayout->addWidget(label, 0, 0);
	
	QWidget *buttonsWidget = new QWidget(this);
	buttonsWidget->setContentsMargins(0, 0, 0, 0);
	layout->addWidget(buttonsWidget);

	QGridLayout *buttonsWidgetLayout = new QGridLayout(buttonsWidget);
	buttonsWidgetLayout->setContentsMargins(0, 0, 12, 0);
	buttonsWidget->setLayout(buttonsWidgetLayout);
	
	_fileNameContainer = new QWidget(this);
	_fileNameContainer->hide();
	_fileNameContainer->setObjectName("browseContainer");
	layout->addWidget(_fileNameContainer);

	QHBoxLayout *saveLayout = new QHBoxLayout(_fileNameContainer);
	_fileNameContainer->setLayout(saveLayout);
	
	QWidget *line;

	line = new QWidget(this);
	line->setFixedHeight(1);
	line->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	line->setStyleSheet("QWidget { background-color: #A3A4A5 ; }");
	layout->addWidget(line);
		
	_model = new FSBMExamples(this,  FSBMExamples::rootelementname );

	_fsBrowser = new FSBrowser(this);
	_fsBrowser->setFSModel(_model);
	layout->addWidget(_fsBrowser);
	
	_breadCrumbs = new BreadCrumbs(buttonsWidget);
	_breadCrumbs->setModel(_model);
	_breadCrumbs->setSeperator(QDir::separator());
	buttonsWidgetLayout->addWidget(_breadCrumbs, 0, 0);
	
	_currentFileName = "";
		
	connect(_fsBrowser, SIGNAL(entryOpened(QString)), this, SLOT(notifyDataSetOpened(QString)));
	
}

void BackstageExamples::notifyDataSetOpened(QString path)
{
	
	FileEvent *event = new FileEvent(this);
	event->setPath(path);
	event->setReadOnly();

	emit dataSetIORequest(event);

}
