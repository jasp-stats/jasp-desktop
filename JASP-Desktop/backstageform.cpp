#include "backstageform.h"
#include "ui_backstageform.h"

#include <QFileDialog>
#include <QStandardPaths>

#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>

#include <QTimer>

#include <QDebug>

#include "appdirs.h"

using namespace std;

BackStageForm::BackStageForm(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::BackStageForm)
{
	_log = NULL;

	ui->setupUi(this);

	//connect(ui->buttonOpen, SIGNAL(clicked()), this, SLOT(openFile()));
	//connect(ui->buttonClose, SIGNAL(clicked()), this, SLOT(closeItemSelected()));
	//connect(ui->buttonExport, SIGNAL(clicked()), this, SLOT(exportItemSelected()));

	//connect(ui->buttonSaveAs, SIGNAL(clicked()), this, SLOT(saveAs()));
	//connect(ui->buttonSave, SIGNAL(clicked()), this, SLOT(save()));

	//connect(ui->recentDataSets, SIGNAL(dataSetSelected(QString)), this, SLOT(recentSelectedHandler(QString)));
	//connect(ui->exampleDataSets, SIGNAL(dataSetSelected(QString)), this, SLOT(exampleSelectedHandler(QString)));

	setFileLoaded(false, NULL);

	QTimer::singleShot(200, this, SLOT(loadExamples())); // delay loading for quick start up

	this->installEventFilter(this);

#ifdef __WIN32__
	QFont f = ui->recentDataSetsHeading->font();
	QFont nf(f.family(), 11, f.weight(), f.italic());
	ui->recentDataSetsHeading->setFont(nf);
	ui->exampleDataSetsHeading->setFont(nf);
#endif

	//ui->buttonNew->hide();

	/*ui->fileMenu->setTabStyleSheet("\
		QPushButton \
		{ \
			background-color: transparent ; \
			border : 1px solid transparent; \
			padding: 12px ; \
			text-align: left ; \
		} \
		 \
		QPushButton::hover \
		{ \
			background-color: rgb(227, 225, 226) ; \
			border : 1px solid rgb(207, 205, 206) ; \
		} \
		 \
		QPushButton::checked \
		{ \
			background-color: rgb(220, 218, 219) ; \
			border: 1px solid #B0B0B0 ; \
		}");*/


	//ui->fileMenu->addTab("New", QIcon(":/icons/document-new.png"));
	ui->fileMenu->addTab("Open", QIcon(":/icons/document-open.png"));
	ui->fileMenu->addTab("Save As", QIcon(":/icons/document-save-as.png"));
	ui->fileMenu->addTab("Save", QIcon(":/icons/document-save.png"));
	ui->fileMenu->addTab("Export");
	ui->fileMenu->addTab("Close", QIcon(":/icons/dialog-close.png"));


	/*ui->openMenu->setTabStyleSheet("\
		QPushButton \
		{ \
			background-color: transparent ; \
			border : 1px solid transparent; \
			padding: 12px ; \
			text-align: left ; \
			height: 32px ; \
			font-size: 16px ; \
		} \
		 \
		QPushButton::hover \
		{ \
			background-color: rgb(227, 225, 226) ; \
			border : 1px solid rgb(207, 205, 206) ; \
		} \
		 \
		QPushButton::checked \
		{ \
			background-color: rgb(220, 218, 219) ; \
			border: 1px solid #B0B0B0 ; \
		}");*/

	ui->openMenu->addTab("Recent");
	ui->openMenu->addTab("Computer");
	ui->openMenu->addTab("OSF", QIcon(":/icons/logo-osf.png"));
	ui->openMenu->addTab("Examples");
}

BackStageForm::~BackStageForm()
{
	delete ui;
}

bool BackStageForm::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::Show || event->type() == QEvent::WindowActivate)
		loadRecents();

	return QWidget::eventFilter(object, event);
}

void BackStageForm::setLog(ActivityLog *log)
{
	_log = log;
}

void BackStageForm::setFileLoaded(bool loaded, QString filename)
{
	_loaded = loaded;
	_filename = filename;

	//ui->buttonClose->setEnabled(loaded);
	//ui->buttonExport->setEnabled(loaded);
	//ui->buttonSaveAs->setEnabled(loaded);

	//if (filename == NULL)
	//	ui->buttonSave->setEnabled(false);
	//else
	//{
	//	QFileInfo fileInfo(filename);
	//	ui->buttonSave->setEnabled(loaded && fileInfo.completeSuffix().compare("jasp") == 0);
	//}
}

void BackStageForm::openFile()
{
	_settings.sync();
	QString path = _settings.value("openPath", QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first()).toString();

#ifdef QT_NO_DEBUG
	QString filename = QFileDialog::getOpenFileName(this, tr("Open Data File"), path, tr("Data files (*.jasp *.csv)"));
#else
	QString filename = QFileDialog::getOpenFileName(this, tr("Open Data File"), path, tr("Data files (*.jasp *.csv *.sav)"));
#endif

	if ( ! filename.isNull() && QFile::exists(filename))
	{
		QFileInfo f(filename);
		path = f.absolutePath();
		_settings.setValue("openPath", path);

		addToRecentList(filename);

		emit dataSetSelected(filename);

		if (_log != NULL)
			_log->log("Open File");
	}
}

void BackStageForm::closeItemSelected()
{
	if (_log != NULL)
		_log->log("Close File");

	emit closeDataSetSelected();
}

void BackStageForm::exportItemSelected()
{
	_settings.sync();
	QString path = _settings.value("openPath", QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first()).toString() + QDir::separator() + "Results.html";
	QString filename = QFileDialog::getSaveFileName(this, tr("Export as HTML"), path, tr("HTML Files (*.html)"));

	if ( ! filename.isNull())
	{
		QFileInfo f(filename);
		path = f.absolutePath();
		_settings.setValue("openPath", path);
		_settings.sync();

		emit exportSelected(filename);

		if (_log != NULL)
			_log->log("Export Results");
	}
}


bool BackStageForm::saveAs()
{
	_settings.sync();

	QString name = "default.jasp";
	if ( ! _filename.isEmpty())
		name = QFileInfo(_filename).baseName();

	QString path = _settings.value("savePath", QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first()).toString() + QDir::separator() + name + ".jasp";

	QString filename = QFileDialog::getSaveFileName(this, tr("Save workspace"), path, tr("JASP Files (*.jasp)"));

	if ( ! filename.isNull())
	{
			QFileInfo f(filename);
			path = f.absolutePath();
			_settings.setValue("savePath", path);
			_settings.sync();

			setFileLoaded(_loaded, filename);

			addToRecentList(filename);

			emit saveSelected(_filename);

			if (_log != NULL)
				_log->log("Save workspace");

			return true;
	}

	return false;
}

bool BackStageForm::save()
{
	bool success = false;
	if (_filename.isEmpty() || QFileInfo(_filename).completeSuffix().compare("jasp") != 0)
		success = saveAs();
	else
	{
		emit saveSelected(_filename);

		if (_log != NULL)
			_log->log("Save workspace");

		success = true;
	}

	return success;
}


void BackStageForm::exampleSelectedHandler(QString path)
{
	qDebug() << QFileInfo(path).baseName();

	if (_log != NULL)
		_log->log("Open Example", QFileInfo(path).baseName());

	emit dataSetSelected(path);
}

void BackStageForm::recentSelectedHandler(QString path)
{
	if (_log != NULL)
		_log->log("Open Recent");

	addToRecentList(path);
	emit dataSetSelected(path);
}

void BackStageForm::loadRecents()
{
	_settings.sync();

	QVariant v = _settings.value("recentItems");
    if (v.type() != QVariant::StringList && v.type() != QVariant::String)
	{
        // oddly, under linux, loading a setting value of type StringList which has
        // only a single string in it, gives you just a string. we QVariant::String is acceptable too

		qDebug() << "BackStageForm::loadRecents();  setting 'recentItems' is not a QStringList";
		return;
	}

	QStringList recents = v.toStringList();

	for (int i = 0; i < recents.size(); i++)
	{
		if (!QFileInfo::exists(recents[i]))
		{
			recents.removeAt(i);
			i -= 1;
		}
	}

	while (_recents.size() > 5)
		_recents.removeFirst();

	if (recents != _recents)
	{
		_recents = recents;
		//ui->recentDataSets->setDataSets(_recents);
	}
}

void BackStageForm::loadExamples()
{
	QFile index(AppDirs::examples() + QDir::separator() + "index.json");

	if ( ! index.exists())
	{
		qDebug() << "BackStageForm::loadExamples();  index not found\n";
		return;
	}

	index.open(QFile::ReadOnly);
	if ( ! index.isOpen())
	{
		qDebug() << "BackStageForm::loadExamples();  index could not be opened\n";
		return;
	}

	QByteArray bytes = index.readAll();
	QJsonParseError error;

	QJsonDocument doc = QJsonDocument::fromJson(bytes, &error);

	if (error.error != QJsonParseError::NoError)
	{
		qDebug() << "BackStageForm::loadExamples();  JSON parse error : " << error.errorString() << "\n";
		return;
	}

	QJsonArray examples = doc.array();

	for (int i = examples.size() - 1; i >= 0; i--)
	{
		QJsonObject example = examples.at(i).toObject();
		QString path = AppDirs::examples() + QDir::separator() + example["path"].toString();
		QString name = example["name"].toString();
		QString description = example["description"].toString();

		//ui->exampleDataSets->addDataSetOption(path, name, description);
	}

}

void BackStageForm::addToRecentList(QString path)
{
	loadRecents();
	_recents.removeAll(path);

	_recents.prepend(path);
	while (_recents.size() > _maxRecents)
		_recents.removeLast();
	_settings.setValue("recentItems", _recents);

	//ui->recentDataSets->setDataSets(_recents);

	_settings.sync();
}



