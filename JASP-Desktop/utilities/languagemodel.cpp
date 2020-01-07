#include "languagemodel.h"
#include <QLocale>
#include "log.h"
#include "utilities/settings.h"
#include <QMap>
#include <QMapIterator>
#include <QDebug>

#include <QQmlEngine>
#include <QQmlApplicationEngine>
#include <QQuickView>
#include <QDir>
#include <locale>
#include "gui/messageforwarder.h"
#include "dirs.h"
#include "qutils.h"
#include <QDirIterator>


using namespace std;

LanguageInfo  LanguageModel::CurrentLanguageInfo;
LanguageModel * LanguageModel::_singleton = nullptr;

LanguageModel::LanguageModel(QString qmresourcepath, QApplication *app, QQmlApplicationEngine *qml, QObject *parent)
	: QAbstractListModel(parent)
{
	assert(!_singleton);

	_singleton = this;

	_mApp = app;
	_parent = parent;
	_qml = qml;
	_mTransLator = new QTranslator(parent);
	_qmlocation = tq(Dirs::resourcesDir()) + qmresourcepath;
	_languages.clear();

	initialize();
}

void LanguageModel::initialize()
{

	//Support English as native JASP language
	_languages.push_back(QLocale::English);  // 31
	QLocale loc(QLocale::English);

	// No language file needed for English (only to show in dropdown in preferences languages)
	//_languagesInfo[QLocale::English] = LanguageInfo (QLocale::English, "English", "English", loc.name(), "", _qmlocation);
	_languagesInfo[QLocale::English] = LanguageInfo (QLocale::English, "English", "English", "en", "", _qmlocation);


	//Default values are now:
	//loc.name();				//en_US
	//loc.nativeCountryName();	//United States
	//loc.nativeLanguageName(); //American English

	findQmFiles(_qmlocation);

	QLocale::Language prefLanguage = static_cast<QLocale::Language>(Settings::value(Settings::PREFERRED_LANGUAGE).toInt());
	LanguageInfo & li = _languagesInfo[prefLanguage];
	CurrentLanguageInfo = li;

	if (prefLanguage == 0 || prefLanguage == QLocale::English) // No preferred language yet set or native JASP language English
	{
		Settings::setValue(Settings::PREFERRED_LANGUAGE, QLocale::English);
		setCurrentIndex(0);
	}
	else
	{
		setCurrentIndex(_languages.indexOf(prefLanguage)); //Update the PrefAdvanced info

		// Load all translated language files for specific language
		loadQmFilesForLanguage(li.language);

		_qml->retranslate();
	}

}

QString LanguageModel::currentLanguageCode() const
{
	return getLocalName(_languages[currentIndex()]);
}

int LanguageModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.

	//if (parent.isValid())
	//	return 0;

	return _languages.size();

}

QVariant LanguageModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	QLocale::Language cl =  _languages[index.row()];

	QString result;
	switch(role)
	{
	case NameRole:			result = getNativeLanguaName(cl); break;
	case LabelRole:			result = getNativeLanguaName(cl); break;
	case ValueRole:			result = getNativeLanguaName(cl); break;
	case NationFlagRole:	result = "qrc:/translations/images/flag_nl.png"; break;
	case LocalNameRole:		result = getLocalName(cl); break;
	default: result = "";
	}

	return result;

}

QHash<int, QByteArray> LanguageModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	NameRole,		"name"					},
		{	LabelRole,		"label"					},
		{	ValueRole,		"value"					},
		{	NationFlagRole,	"iconfile"				},
		{	LocalNameRole,	"languageIdentifier"	}
	};

	return roles;

}

void LanguageModel::changeLanguage(int index)
{	
	// Called from PrefsUI.qml

	QLocale::Language cl = _languages[index];
	if (CurrentLanguageInfo.language == cl)
		return; //No change of language

	LanguageInfo & li = _languagesInfo[cl];
	CurrentLanguageInfo = li;

	if (cl == QLocale::English)
	{
		removeTranslators();
		_qml->retranslate();

	}
	else
	{
		loadQmFilesForLanguage(li.language);
		_qml->retranslate();
	}

	Settings::setValue(Settings::PREFERRED_LANGUAGE, cl);
	setCurrentIndex(index);
	emit languageChanged();

}

QString LanguageModel::getLocalName(QLocale::Language cl) const
{
	LanguageInfo li = _languagesInfo[cl];
	return li.localName;

}

QString LanguageModel::getNativeLanguaName(QLocale::Language cl) const
{
	LanguageInfo li = _languagesInfo[cl];
	return li.nativeLanguageName;

}

QLocale::Language LanguageModel::getLanguageKeyFromName(QString languagename) const
{
	QMapIterator<QLocale::Language, LanguageInfo> it(_languagesInfo) ;

	while (it.hasNext())
	{
		it.next();
		QLocale::Language l =  it.key();
		LanguageInfo li =_languagesInfo[l];
		if (languagename == li.nativeLanguageName) return l;
	}
	return QLocale::English; //By default

}

QString  LanguageModel::getLocalNameFromQmFileName(QString filename) const
{

	int start = filename.indexOf('_');
	QString localname = filename.mid(start + 1);

	int end = localname.lastIndexOf('.');
	localname = localname.left(end);

	return localname;

}

bool LanguageModel::isValidLocalName(QString filename, QLocale::Language & cl)
{

	//Checks if the filename has a proper localename suffix for a valid CLocale::Language
	//https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
	//We use the two lettecode in the qm filename

	QString localname = getLocalNameFromQmFileName(filename);
	QLocale loc(localname);
	QString languagename = "";
	languagename = loc.nativeLanguageName();
	if (languagename == "")
		return false;
	cl = loc.language();
	return true;

}

bool LanguageModel::isValidLocalName(QString filename, QLocale & local)
{
	//Checks if the filename has a proper localename suffix for a valid CLocale::Language
	//https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
	//We use the two lettecode in the qm filename

	QString localname = getLocalNameFromQmFileName(filename);
	QLocale loc(localname);
	QString languagename = "";
	languagename = loc.nativeLanguageName();
	if (languagename == "")
		return false;

	local = loc;
	return true;

}

bool LanguageModel::isJaspSupportedLanguage(QLocale::Language lang)
{
	if (find(_languages.begin(), _languages.end(), lang) != _languages.end()) return true;
	return false;

}

void LanguageModel::setCurrentIndex(int currentIndex)
{
	if (_currentIndex == currentIndex)
		return;

	_currentIndex = currentIndex;
	emit currentIndexChanged();

}

void LanguageModel::loadModuleTranslationFile(Modules::DynamicModule *dyn)
{
	bool result;
	QLocale loc;
	QLocale::Language cl;
	bool newfileloaded = false;

	//Get qm folder as subfolder from qml folder
	QString qmFolder = QString::fromStdString(dyn->qmlFilePath("")) + "qm";

	QDirIterator qdi(qmFolder, QStringList() << "*.qm" << "*.QM");

	while (qdi.hasNext())
	{
		qdi.next();
		QFileInfo fi = qdi.fileInfo();

		//Can QLocale be found from localname suffix?
		if (!isValidLocalName(fi.fileName(), loc))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		cl = loc.language();
		if (!isJaspSupportedLanguage(cl))
		{
			Log::log() << "Not a Jasp supported language in: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		LanguageInfo & li = _languagesInfo[cl];
		QString addFile = fi.filePath();
		li.qmFilenames.push_back(addFile);
		//li.qmFilenames.push_front(addFile);

		if (cl != CurrentLanguageInfo.language)
		{
			//Module language differs from Jasp language. Just add to qmFiles for further use.			
			Log::log() << "This module translation" << dyn->name() << " with " << fi.fileName().toStdString() << "does not support the current language "<<  CurrentLanguageInfo.languageName << std::endl ;
		}
		else
		{
			newfileloaded = true;
			loadQmFile(fi.filePath());
		}
	}
	if (newfileloaded)
	{
		result = _mApp->installTranslator(_mTransLator);
		_qml->retranslate();
	}

}

void LanguageModel::findQmFiles(QString qmlocation)
{	

	QDir dir(qmlocation);
	QLocale loc;
	QLocale::Language cl;

	QDirIterator qdi(qmlocation, QStringList() << "*.qm" << "*.QM");

	while (qdi.hasNext())
	{
		qdi.next();

		QFileInfo fi = qdi.fileInfo();

//		qDebug() << "fileName : " << fi.fileName() << endl; //e.g. jasp_nl.qm
//		qDebug() << "baseName : " << fi.baseName() << endl; //e.g. jasp_nl
//		qDebug() << "path : " << fi.path() << endl; //Library/Application..
//		qDebug() << "full filename " << fi.filePath() << endl; // e.g. /Users/fransmeerhoff/JASP/Develop/build-Debug/Resources/Translations/module_np.qm
//		qDebug() << "absoluteFilePath : " << fi.absoluteFilePath() << endl; // e.g. /Users/fransmeerhoff/JASP/Develop/build-Debug/Resources/Translations/module_np.qm
//		qDebug() << "absolutePath " << fi.absolutePath() << endl; //e.g. /Users/fransmeerhoff/JASP/Develop/build-Debug/Resources/Translations
//		qDebug() << "absoluteDir" << fi.absoluteDir() << endl; //e.g. QDir( "/Users/fransmeerhoff/JASP/Develop/build-Debug/Resources/Translations" , nameFilters = { "*" },  QDir::SortFlags( Name | IgnoreCase ) , QDir::Filters( Dirs|Files|Drives|AllEntries ) )

		QString localname = getLocalNameFromQmFileName(fi.fileName());

		//Can QLocale be found from localname suffix e.g. _nl?
		if (!isValidLocalName(fi.fileName(), loc))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;

		}

		if (count(_languages.begin(), _languages.end(), loc.language()) == 0)
		{
			//First time new language found
			_languages.push_back(loc.language());
			_languagesInfo[loc.language()] = LanguageInfo(loc.language(), QLocale::languageToString(loc.language()), loc.nativeLanguageName(), localname, fi.filePath(), qmlocation);
		}
		else
			//More translated language files for a language
			_languagesInfo[loc.language()].qmFilenames.push_back(fi.filePath());
	}

}

void LanguageModel::loadQmFilesForLanguage(QLocale::Language cl)
{
	LanguageInfo & li = _languagesInfo[cl];
	bool result;
	for (QString qmfilename: li.qmFilenames)
		loadQmFile(qmfilename);

}

void LanguageModel::loadQmFile(QString filename)
{
	QFileInfo fi(filename);

	QTranslator *qtran = new QTranslator();

	if (!qtran->load(filename))
	{
		Log::log() << "Unable to load translation file: " << fi.filePath()  << std::endl ;
		delete qtran;
		return;
	}

	_translators.push_back(qtran);
	_mApp->installTranslator(qtran);

}

void LanguageModel::removeTranslators()
{
	for (QTranslator *qtran: _translators)
	{
		_mApp->removeTranslator( qtran);
		delete qtran;
	}
	_translators.clear();

}

int LanguageModel::currentIndex() const
{
	return _currentIndex;

}

QString LanguageModel::getCurrentLanguageFileExtension()
{
	return CurrentLanguageInfo.localName;
}

void LanguageModel::setApplicationEngine(QQmlApplicationEngine * ae)
{
	_qml = ae;

}




