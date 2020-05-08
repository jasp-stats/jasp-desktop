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


LanguageInfo::LanguageInfo()
{
	Log::log() << "new LanguageInfo created: " << toString() << std::endl;
}

LanguageInfo::LanguageInfo(QLocale::Language language, QString languageName, QString nativeLanguageName, QString localName, QString qmFilename, QString qmFolder)
	: language(language), languageName(languageName), nativeLanguageName(nativeLanguageName), localName(localName), qmFolder(qmFolder), qmFilenames({qmFilename})
{
	Log::log() << "new LanguageInfo created: " << toString() << std::endl;
}

QString LanguageInfo::toString()
{
	return "Language: '" + languageName + "' qmFolder: '" + qmFolder + "' and qmFilenames: '" + qmFilenames.toList().join(", ") + "'";
}

LanguageModel * LanguageModel::_singleton = nullptr;

LanguageModel::LanguageModel(QApplication *app, QQmlApplicationEngine *qml, QObject *parent)
	: QAbstractListModel(parent),
	  _mApp(app),
	  _mTranslator(new QTranslator(this)),
	  _qml(qml)
{
	assert(!_singleton);

	_singleton = this;
	_qmlocation = tq(Dirs::resourcesDir()) + "Translations";

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
	_currentLanguageInfo = li;

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
	Log::log() << "LanguageModel::changeLanguage(int index = " << index << ") called." << std::endl;
	// Called from PrefsUI.qml

	QLocale::Language cl = _languages[index];
	if (currentLanguageInfo().language == cl) //We could have just checked against _currentIndex right?
	{
		Log::log() << "Language already selected..." << std::endl;
		return; //No change of language
	}
	_currentLanguageInfo = _languagesInfo[cl];

	if (cl == QLocale::English)	removeTranslators();
	else						loadQmFilesForLanguage(currentLanguageInfo().language);


	_qml->retranslate();
	Settings::setValue(Settings::PREFERRED_LANGUAGE, cl);
	setCurrentIndex(index);
	_shouldEmitLanguageChanged = true;
}

void LanguageModel::resultsPageLoaded()
{
	if(!_shouldEmitLanguageChanged)
		return;

	_shouldEmitLanguageChanged = false;
	emit languageChanged();
}

QString LanguageModel::getLocalName(QLocale::Language cl) const
{
	return _languagesInfo[cl].localName;
}

QString LanguageModel::getNativeLanguaName(QLocale::Language cl) const
{
	return _languagesInfo[cl].nativeLanguageName;
}

QString  LanguageModel::getLocalNameFromQmFileName(QString filename) const
{
	int		start		= filename.indexOf('_');
	QString localname	= filename.mid(start + 1);
	int		end			= localname.lastIndexOf('.');

	return localname.left(end);

}

bool LanguageModel::isValidLocalName(QString filename, QLocale::Language & cl)
{
	QLocale loc;

	if(!isValidLocalName(filename, loc))
		return false;

	cl = loc.language();
	return true;

}

bool LanguageModel::isValidLocalName(QString filename, QLocale & local)
{
	//Checks if the filename has a proper localename suffix for a valid CLocale::Language
	//https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
	//We use the two lettecode in the qm filename

	QLocale loc			 = getLocalNameFromQmFileName(filename);
	QString languagename = loc.nativeLanguageName();

	if (languagename == "")
		return false;

	local = loc;
	return true;

}


void LanguageModel::setCurrentIndex(int currentIndex)
{
	if (_currentIndex == currentIndex)
		return;

	Log::log() << "LanguageModel::setCurrentIndex changed index from " << _currentIndex << " to " << currentIndex << std::endl;
	_currentIndex = currentIndex;
	emit currentIndexChanged();

}

void LanguageModel::loadModuleTranslationFile(Modules::DynamicModule *dyn)
{
	Log::log() << "LanguageModel::loadModuleTranslationFile called for module: " << (dyn ? dyn->name() : "NULL") << std::endl;

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

		if (cl != currentLanguageInfo().language)
		{
			//Module language differs from Jasp language. Just add to qmFiles for further use.			
			Log::log() << "This module translation" << dyn->name() << " with " << fi.fileName().toStdString() << "does not support the current language "<<  currentLanguageInfo().languageName << std::endl ;
		}
		else
		{
			newfileloaded = true;
			loadQmFile(fi.filePath());
		}
	}
	if (newfileloaded)
	{
		result = _mApp->installTranslator(_mTranslator);
		_qml->retranslate();
	}

}

void LanguageModel::findQmFiles(QString qmlocation)
{	
	Log::log() << "findQmlFiles(qmlocation = " << qmlocation << ") called" << std::endl;

	QDir dir(qmlocation);
	QLocale loc;

	QDirIterator qdi(qmlocation, QStringList() << "*.qm" << "*.QM");

	if(!qdi.hasNext())
		throw std::runtime_error("Could not find *any* qml-files!");

	while (qdi.hasNext())
	{
		qdi.next();

		QFileInfo fi = qdi.fileInfo();

		Log::log() << "Checking qm file: " << fi.absoluteFilePath() << std::endl;

		QString localname = getLocalNameFromQmFileName(fi.fileName());

		//Can QLocale be found from localname suffix e.g. _nl?
		if (!isValidLocalName(fi.fileName(), loc))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;

		}

		if (_languages.count(loc.language()) == 0)
		{
			Log::log() << "Language (" << loc.language() << ") not registered in LanguageModel, adding it now" << std::endl;

			_languages.push_back(loc.language());
			_languagesInfo.insert(loc.language(), LanguageInfo(loc.language(), QLocale::languageToString(loc.language()), loc.nativeLanguageName(), localname, fi.filePath(), qmlocation));
		}
		else
		{
			Log::log() << "More translated language files for a language that was already register in LanguageModel" << std::endl;
			_languagesInfo[loc.language()].qmFilenames.push_back(fi.filePath());
		}
	}

}

void LanguageModel::loadQmFilesForLanguage(QLocale::Language cl)
{
	Log::log() << "LanguageModel::loadQmFilesForLanguage(QLocale::Language cl=" << cl << ")" << std::endl;

	LanguageInfo & li = _languagesInfo[cl];

	for (QString qmfilename: li.qmFilenames)
		loadQmFile(qmfilename);

}

void LanguageModel::loadQmFile(QString filename)
{
	Log::log() << "loadQmFile(" << filename << ")" << std::endl;

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
	Log::log() << "LanguageModel::removeTranslators()" << std::endl;

	for (QTranslator *qtran: _translators)
	{
		_mApp->removeTranslator( qtran);
		delete qtran;
	}
	_translators.clear();

}

QString LanguageModel::currentLanguageCode() const
{
	if(currentIndex() >= _languages.size())
	{
		Log::log() << "LanguageModel::currentLanguageCode() is looking for a language with an index (" << currentIndex() << ") that is higher then the nr of languages available (" << _languages.size() << ") returning the last one instead." << std::endl;
		return getLocalName(_languages[_languages.size() -1]);
	}
	else
		return getLocalName(_languages[currentIndex()]);
} //Here we use currentIndex instead of CurrentLanguageInfo? Why not everywhere else?
