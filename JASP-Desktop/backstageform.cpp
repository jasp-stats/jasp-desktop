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
    ui->setupUi(this);

	connect(ui->buttonOpen, SIGNAL(clicked()), this, SLOT(fileItemSelected()));
	connect(ui->buttonClose, SIGNAL(clicked()), this, SLOT(closeItemSelected()));
	connect(ui->buttonExport, SIGNAL(clicked()), this, SLOT(exportItemSelected()));

	connect(ui->recentDataSets, SIGNAL(dataSetSelected(QString)), this, SLOT(recentSelectedHandler(QString)));
	connect(ui->exampleDataSets, SIGNAL(dataSetSelected(QString)), this, SLOT(exampleSelectedHandler(QString)));

	setFileLoaded(false);

	QTimer::singleShot(200, this, SLOT(loadExamples())); // delay loading for quick start up

	this->installEventFilter(this);

#ifdef __WIN32__
	QFont f = ui->recentDataSetsHeading->font();
	QFont nf(f.family(), 11, f.weight(), f.italic());
	ui->recentDataSetsHeading->setFont(nf);
	ui->exampleDataSetsHeading->setFont(nf);
#endif
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

void BackStageForm::setFileLoaded(bool loaded)
{
	ui->buttonClose->setEnabled(loaded);
	ui->buttonExport->setEnabled(loaded);
}

void BackStageForm::fileItemSelected()
{
	_settings.sync();
	QString path = _settings.value("openPath", QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first()).toString();
	QString filename = QFileDialog::getOpenFileName(this, tr("Open CSV File"), path, tr("CSV Files (*.csv)"));

	if ( ! filename.isNull() && QFile::exists(filename))
	{
		QFileInfo f(filename);
		path = f.absolutePath();
		_settings.setValue("openPath", path);

		addToRecentList(path);

		emit dataSetSelected(filename);
	}
}

void BackStageForm::closeItemSelected()
{
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
	}
}

void BackStageForm::exampleSelectedHandler(QString path)
{
	emit dataSetSelected(path);
}

void BackStageForm::recentSelectedHandler(QString path)
{
	addToRecentList(path);
	emit dataSetSelected(path);
}

void BackStageForm::loadRecents()
{
	_settings.sync();

	QVariant v = _settings.value("recentItems");
	if (v.type() != QVariant::StringList)
	{
		qDebug() << "BackStageForm::loadRecents();  setting 'recentItems' is not a QStringList";
		return;
	}

	QStringList recents = v.toStringList();

	while (_recents.size() > 5)
		_recents.removeFirst();

	if (recents != _recents)
	{
		_recents = recents;
		ui->recentDataSets->setDataSets(_recents);
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

		ui->exampleDataSets->addDataSetOption(path, name, description);
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

	ui->recentDataSets->setDataSets(_recents);

	_settings.sync();
}



