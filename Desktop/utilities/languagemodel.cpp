#include "languagemodel.h"
#include "log.h"
#include "utilities/settings.h"
#include <QDebug>
#include <QDir>
#include "dirs.h"
#include "utilities/qutils.h"
#include <QDirIterator>
#include "results/resultsjsinterface.h"
#include "modules/dynamicmodule.h"

LanguageModel * LanguageModel::_singleton = nullptr;
QLocale LanguageModel::_defaultLocale = QLocale(QLocale::English, QLocale::World);

QMap<QString, bool> LanguageModel::LanguageInfo::_allowedLanguages =
{
	{ "en"		,	true	},
	{ "nl"		,	true	},
	{ "de"		,	true	},
	{ "pt"		,	true	},
	{ "gl"		,	true	},
	{ "ja"		,	true	},
	{ "es"		,	true	},
	{ "zh_Hans"	,	true	},
	{ "zh_Hant" ,	false	},
	{ "id"		,	false	},
	{ "fr"		,	false   },
	{ "ru"		,	false	}
};
QString LanguageModel::LanguageInfo::_incompleteFlag = "(incomplete)";

QString LanguageModel::LanguageInfo::getLanguageCode(const QLocale& locale)
{
	return locale.name().split("_")[0];
}

LanguageModel::LanguageInfo::LanguageInfo(const QLocale& _locale, const QString& _code, const QString& _qmFilename)
	 : locale(_locale), code(_code)
{
	entryName =  code + " - " + locale.nativeLanguageName();

	if (!_allowedLanguages[code])	entryName += (" " + _incompleteFlag);
	if (!_qmFilename.isEmpty())		qmFilenames.push_back(_qmFilename);
}

LanguageModel::LanguageModel(QApplication *app, QQmlApplicationEngine *qml, QObject *parent)
	: QAbstractListModel(parent),
	  _mApp(app),
	  _mTranslator(new QTranslator(this)),
	  _qml(qml)
{
	assert(!_singleton);

	_singleton = this;
	_qmLocation = tq(Dirs::resourcesDir()) + "Translations";

	initialize();
}

void LanguageModel::initialize()
{
	QString defaultLanguageCode = LanguageInfo::getLanguageCode(_defaultLocale);
	LanguageInfo defaultLanguageInfo(_defaultLocale);
	_languages[defaultLanguageCode] = defaultLanguageInfo;

	findQmFiles();

	_currentLanguageCode = Settings::value(Settings::PREFERRED_LANGUAGE).toString();
	if (!LanguageInfo::isLanguageAllowed(_currentLanguageCode)) _currentLanguageCode = defaultLanguageCode;

	if (_currentLanguageCode != defaultLanguageCode)
	{
		// Load all translated language files for specific language
		loadQmFilesForLanguage(_currentLanguageCode);
		_qml->retranslate();
	}

}

QVariant LanguageModel::data(const QModelIndex &index, int role) const
{
	if (index.row() < 0 || index.row() >= rowCount())
		return QVariant();

	QString languageCode = _languages.keys()[index.row()];

	QString result;
	switch(role)
	{
	case NameRole:
	case Qt::DisplayRole:
	case LabelRole:
	case ValueRole:			result = _languages[languageCode].entryName; break;
	case NationFlagRole:	result = "qrc:/translations/images/flag_" + languageCode + ".png"; break;
	case LocalNameRole:		result = languageCode; break;
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

void LanguageModel::setCurrentLanguage(QString language)
{	
	QString languageCode = language.split(" ")[0];
	if (languageCode == _currentLanguageCode || languageCode.isEmpty() || !_languages.contains(languageCode))
		return;

	_currentLanguageCode = languageCode;

	if (_currentLanguageCode == LanguageInfo::getLanguageCode(_defaultLocale))	removeTranslators();
	else																		loadQmFilesForLanguage(_currentLanguageCode);

	//prepare for language change
	emit aboutToChangeLanguage();								//asks all analyses to abort and to block refresh
	ResultsJsInterface::singleton()->setResultsLoaded(false);	//So that javascript starts queueing any Js (such as title changed of an analysis) until the page is reloaded

	//On linux it somehow ignores the newer settings, so instead of pausing we kill the engines... https://github.com/jasp-stats/jasp-test-release/issues/1046
	//But I do not know if it necessary, because the modules-translations aren't working.
#ifdef __gnu_linux__
	emit stopEngines();
#else
	emit pauseEngines();										//Hopefully avoids process being called while we are in the middle of changing the language
#endif

	_qml->retranslate();
	Settings::setValue(Settings::PREFERRED_LANGUAGE , _currentLanguageCode);
	Settings::setValue(Settings::PREFERRED_COUNTRY, _languages[_currentLanguageCode].locale.country());
	_shouldEmitLanguageChanged = true;

	ResultsJsInterface::singleton()->resetResults();
	
	//resumeEngines() will be emitted in resultsPageLoaded
}

void LanguageModel::resultsPageLoaded()
{
	if(!_shouldEmitLanguageChanged)
		return;
	
	_shouldEmitLanguageChanged = false;
	emit currentLanguageChanged();
	emit resumeEngines();
}

bool LanguageModel::isValidLocaleName(const QString& filename, QLocale& locale, QString& languageCode)
{
	//Checks if the filename has a proper localename suffix for a valid CLocale::Language
	//https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
    //We use the two letter code in the qm filename

	int		start		= filename.indexOf('-');
	QString localeName	= filename.mid(start + 1);
	int		end			= localeName.lastIndexOf('.');

	languageCode = localeName.left(end);
	locale = QLocale(languageCode);

	return LanguageInfo::isLanguageAllowed(languageCode);
}

void LanguageModel::loadModuleTranslationFiles(Modules::DynamicModule *dyn)
{
	//Log::log() << "LanguageModel::loadModuleTranslationFile called for module: " << (dyn ? dyn->name() : "NULL") << std::endl;

	bool result;
	QLocale loc;
	QString languageCode;
	bool newfileloaded = false;

	//Get qm folder as subfolder from qml folder
	QString qmFolder = QString::fromStdString(dyn->qmlFilePath("")) + "translations";

	QDirIterator qdi(qmFolder, QStringList() << "*.qm" << "*.QM");

	while (qdi.hasNext())
	{
		qdi.next();
		QFileInfo fi = qdi.fileInfo();

		//Can QLocale be found from localname suffix?
		if (!isValidLocaleName(fi.fileName(), loc, languageCode))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		if (!_languages.contains(languageCode))
		{
			Log::log() << "Not a Jasp supported language in: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		LanguageInfo & li = _languages[languageCode];
		QString addFile = fi.filePath();
		li.qmFilenames.push_back(addFile);

		if (languageCode != _currentLanguageCode)
		{
			//Module language differs from Jasp language. Just add to qmFiles for further use.			
			//Log::log() << "The translation for module '" << dyn->name() << "' with " << fi.fileName().toStdString() << " does not support the current language "<<  currentLanguageInfo().languageName << std::endl ;
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

void LanguageModel::findQmFiles()
{	
	QDir dir(_qmLocation);
	QLocale loc;
	QString languageCode;

	QDirIterator qdi(_qmLocation, QStringList() << "*.qm" << "*.QM");

	if(!qdi.hasNext())
		throw std::runtime_error("Could not find *any* qm-files!");

	while (qdi.hasNext())
	{
		qdi.next();

		QFileInfo fi = qdi.fileInfo();

		Log::log() << "Checking qm file: " << fi.absoluteFilePath() << std::endl;

		if (!isValidLocaleName(fi.fileName(), loc, languageCode))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		if (!_languages.contains(languageCode))
		{
			Log::log() << "Language (" << QLocale::languageToString(loc.language()) << ") not registered in LanguageModel, adding it now" << std::endl;
			_languages[languageCode] = LanguageInfo(loc, languageCode, fi.filePath());
		}
		else
		{
			Log::log() << "More translated language files for a language that was already register in LanguageModel" << std::endl;
			_languages[languageCode].qmFilenames.push_back(fi.filePath());
		}
	}

}

void LanguageModel::loadQmFilesForLanguage(const QString& languageCode)
{
	LanguageInfo & li = _languages[languageCode];

	for (QString qmfilename: li.qmFilenames)
		loadQmFile(qmfilename);

}

void LanguageModel::loadQmFile(const QString& filename)
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

QString LanguageModel::currentLanguage() const
{
	const LanguageInfo & li = _languages[_currentLanguageCode];
	return li.entryName;
}

bool LanguageModel::hasDefaultLanguage() const
{
	const LanguageInfo & li = _languages[_currentLanguageCode];
	return li.locale == _defaultLocale;
}
