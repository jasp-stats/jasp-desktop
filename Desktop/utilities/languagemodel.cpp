#include "languagemodel.h"
#include "utilities/qutils.h"
#include "utilities/settings.h"
#include "resultstesting/compareresults.h"
#include "results/resultsjsinterface.h"
#include "modules/dynamicmodule.h"
#include "emptyvalues.h"
#include "columnutils.h"
#include <QDirIterator>
#include <QDebug>
#include "dirs.h"
#include <QDir>
#include "log.h"

LanguageModel * LanguageModel::_singleton	= nullptr;
QLocale LanguageModel::_defaultLocale		= QLocale(QLocale::English, QLocale::UnitedStates);
QLocale LanguageModel::_alternativeLocale	= QLocale(QLocale::English, QLocale::UnitedStates);

// the bool indicates whether the language is considered "complete" or not
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
	{ "zh_Hant" ,	true	},
	{ "fr"		,	true    },
	{ "pl"		,	true	},
	{ "sr"		,	true	},
	{ "id"		,	false	},
	{ "ru"		,	false	},
	{ "it"		,	false	}
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

void LanguageModel::setAlternativeLocaleStatic()
{
	_alternativeLocale = QLocale(_nativeLanguageNameToEnum[_currentAltLanguage], _nativeTerritoryNameToEnum[_currentAltTerritory]);	
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
	
	QString			defaultLanguageCode = LanguageInfo::getLanguageCode(_defaultLocale);
	LanguageInfo	defaultLanguageInfo	= _defaultLocale;
	
	_languages[defaultLanguageCode] = defaultLanguageInfo;

	findQmFiles();

	_currentLanguageCode	= Settings::value(Settings::PREFERRED_LANGUAGE		).toString();
	_useAlternativeLocale	= Settings::value(Settings::USE_ALT_LOCALE			).toBool();
	_currentAltLanguage		= Settings::value(Settings::ALT_LOCALE_LANGUAGE		).toString();
	_currentAltTerritory	= Settings::value(Settings::ALT_LOCALE_REGION		).toString();
	_useThousandSeps		= Settings::value(Settings::USE_THOUSAND_SEPARATORS	).toBool();
	
	fillAltOptions();
	
	if(resultXmlCompare::compareResults::theOne()->testMode()) //in testmode we run usa locale without thousands separators
	{
		_currentLanguageCode	= defaultLanguageCode;
		_useAlternativeLocale	= true;
		_useThousandSeps		= false;
		_currentAltLanguage		= _defaultLocale.nativeLanguageName();
		_currentAltTerritory	= _defaultLocale.nativeTerritoryName();
	}
	
	if (!LanguageInfo::isLanguageAllowed(_currentLanguageCode)) 
		_currentLanguageCode = defaultLanguageCode;

	if (_currentLanguageCode != defaultLanguageCode)
	{
		// Load all translated language files for specific language
		loadQmFilesForLanguage(_currentLanguageCode);
		
		setDefaultLocaleFromCurrent();
		
		_qml->retranslate();
	}
}

void LanguageModel::fillAltOptions()
{
	QList<QLocale> allLocales = QLocale::matchingLocales(
				QLocale::AnyLanguage,
				QLocale::AnyScript,
				QLocale::AnyCountry);
	
	QSet<QString> nativeLanguageNames;
	_nativeLanguageNameToEnum.clear();
	
	for(QLocale & l : allLocales)
	{
		_nativeLanguageNameToEnum[l.nativeLanguageName()] = l.language();
		nativeLanguageNames.insert(l.nativeLanguageName());
	}
	nativeLanguageNames.remove("");
	_altLanguages	= QStringList(nativeLanguageNames.begin(),	nativeLanguageNames.end());
	
	std::sort(_altLanguages.begin(), _altLanguages.end(), [](const QString & l, const QString & r)
	{
		return l.toLower() < r.toLower();
	});
	
	fillAltTerritories();
}

void LanguageModel::fillAltTerritories()
{
	QLocale::Language languageChosen = _currentAltLanguage == "" ? _defaultLocale.language() : _nativeLanguageNameToEnum[_currentAltLanguage];
	
	QList<QLocale> languageLocales = QLocale::matchingLocales(
				languageChosen,
				QLocale::AnyScript,
				QLocale::AnyCountry);
	
	bool				previouslyChosenTerritoryFound	= false;
	QLocale::Territory	prevTer							= _nativeTerritoryNameToEnum.count(_currentAltTerritory) ? _nativeTerritoryNameToEnum.at(_currentAltTerritory) : QLocale::Territory::AnyTerritory;
	
			
	QSet<QString> nativeTerritoryNames;
	_nativeTerritoryNameToEnum.clear();

	for(QLocale & l : languageLocales)
	{
		QString territory = l.nativeTerritoryName();
		
		nativeTerritoryNames.insert(territory);
		_nativeTerritoryNameToEnum [territory] = l.territory();
		
		if(prevTer == l.territory())		
			previouslyChosenTerritoryFound	= true;
	}
	
	nativeTerritoryNames.remove("");
	_altTerritories = QStringList(nativeTerritoryNames.begin(), nativeTerritoryNames.end());
	
	std::sort(_altTerritories.begin(), _altTerritories.end(), [](const QString & l, const QString & r)
	{
		return l.toLower() < r.toLower();
	});
	
	emit altTerritoriesChanged();
	
	
	if(!previouslyChosenTerritoryFound || _currentAltTerritory == "" || prevTer == QLocale::Territory::AnyTerritory)
		_currentAltTerritory = QLocale(languageChosen).nativeTerritoryName();
	else if(prevTer != QLocale::Territory::AnyTerritory)
		_currentAltTerritory = QLocale(languageChosen, prevTer).nativeTerritoryName();
	

	emit currentAltTerritoryChanged();
}

QVariant LanguageModel::data(const QModelIndex &index, int role) const
{
	if (index.row() < 0 || index.row() >= rowCount())
		return QVariant();

	
	QString languageCode = std::next(_languages.begin(), index.row())->first;

	QString result;
	switch(role)
	{
	case NameRole:
	case Qt::DisplayRole:
	case LabelRole:
	case ValueRole:			result = _languages.at(languageCode).entryName; break;
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
	if(resultXmlCompare::compareResults::theOne()->testMode())
		return;
	
	QString languageCode = language.split(" ")[0];
	if (languageCode == _currentLanguageCode || languageCode.isEmpty() || !_languages.count(languageCode))
		return;

	_currentLanguageCode = languageCode;

	if (_currentLanguageCode == LanguageInfo::getLanguageCode(_defaultLocale))	removeTranslators();
	else																		loadQmFilesForLanguage(_currentLanguageCode);

	refreshAll();
}

void LanguageModel::setDefaultLocaleFromCurrent()
{
	setAlternativeLocaleStatic();
	
	QColumnUtils::setCallbacksAndDefaultLocale(currentLocale(), useThousandSeps());
	
	emit currentLocaleChanged(currentLocale().bcp47Name(), useThousandSeps());
	emit exampleFormattingChanged();
}

void LanguageModel::refreshAll()
{
	//prepare for language change
	emit aboutToChangeLanguage();								//asks all analyses to abort and to block refresh
	ResultsJsInterface::singleton()->setResultsLoaded(false);	//So that javascript starts queueing any Js (such as title changed of an analysis) until the page is reloaded
	
	setDefaultLocaleFromCurrent();

	//On linux it somehow ignores the newer settings, so instead of pausing we kill the engines... https://github.com/jasp-stats/jasp-test-release/issues/1046
	//But I do not know if it necessary, because the modules-translations aren't working.
//#ifdef __gnu_linux__
//	emit stopEngines();
//#else
//	emit pauseEngines();										//Hopefully avoids process being called while we are in the middle of changing the language
//#endif

	//Seemingly there are two possible race conditions on translations that can be fixed by just killing the engines
	//Which is fine since retranslation is not a common occurence anyway.
	emit stopEngines();

	_qml->retranslate();
	if(!resultXmlCompare::compareResults::theOne()->testMode())
	{
		Settings::setValue(Settings::PREFERRED_LANGUAGE ,	currentLanguageCode());
		Settings::setValue(Settings::PREFERRED_COUNTRY,		currentLocale().country());
	}
	_shouldEmitLanguageChanged = true;

	ResultsJsInterface::singleton()->resetResults();
	
	//resumeEngines() will be emitted in resultsPageLoaded
	
	emit languageChangeDone();
}

void LanguageModel::setUseAlternativeLocale(bool useIt)
{
	if(_useAlternativeLocale == useIt || resultXmlCompare::compareResults::theOne()->testMode())
		return;
		
	_useAlternativeLocale = useIt;
	
	Settings::setValue(Settings::USE_ALT_LOCALE ,	_useAlternativeLocale);
	
	emit useAlternativeLocaleChanged();
	refreshAll();
}

void LanguageModel::resultsPageLoaded()
{
	if(!_shouldEmitLanguageChanged)
		return;
	
	_shouldEmitLanguageChanged = false;
	emit currentLanguageChanged();
	emit currentLocaleChanged(currentLocale().bcp47Name(), useThousandSeps());
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

		if (!_languages.count(languageCode))
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

		if (!_languages.count(languageCode))
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

	for (const QString & qmfilename: li.qmFilenames)
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
	return _languages.at(_currentLanguageCode).entryName;
}

const QLocale & LanguageModel::currentLocale() const
{
	
	return useAlternativeLocale() ? _alternativeLocale : _languages.at(_currentLanguageCode).locale;
}

bool LanguageModel::hasDefaultLanguage() const
{
	return _languages.at(_currentLanguageCode).locale == _defaultLocale;
}

QString LanguageModel::currentAltLanguage() const
{
	return _currentAltLanguage;
}

void LanguageModel::setCurrentAltLanguage(const QString &newCurrentAltLanguage)
{
	if (_currentAltLanguage == newCurrentAltLanguage)
		return;
	
	_currentAltLanguage = newCurrentAltLanguage;
	emit currentAltLanguageChanged();
	
	Settings::setValue(Settings::ALT_LOCALE_LANGUAGE, _currentAltLanguage);
		
	fillAltTerritories();
	
	refreshAll();
}


QString LanguageModel::currentAltTerritory() const
{
	return _currentAltTerritory;
}

QString LanguageModel::exampleFormatting() const
{
	QLocale cur = currentLocale();
	
	QStringList examples;
	
	examples.push_back(QColumnUtils::doubleToString(1.234567890));
	examples.push_back(QColumnUtils::doubleToString(12345.67890));
	examples.push_back(QColumnUtils::doubleToString(1234567890));
	examples.push_back(QColumnUtils::currencyString(10000000.10, "€"));
	
	return examples.join("\n");
	
}

void LanguageModel::setCurrentAltTerritory(const QString &newCurrentAltTerritory)
{
	if (_currentAltTerritory == newCurrentAltTerritory)
		return;
	
	_currentAltTerritory = newCurrentAltTerritory;
	emit currentAltTerritoryChanged();
	
	Settings::setValue(Settings::ALT_LOCALE_REGION, _currentAltTerritory);
	
	refreshAll();
}

bool LanguageModel::useThousandSeps() const
{
	return _useThousandSeps;
}

void LanguageModel::setUseThousandSeps(bool newUseThousandSeps)
{
	if (_useThousandSeps == newUseThousandSeps)
		return;
	
	_useThousandSeps = newUseThousandSeps;
	emit useThousandSepsChanged();
	emit currentLocaleChanged(currentLocale().bcp47Name(), useThousandSeps());
	
	Settings::setValue(Settings::USE_THOUSAND_SEPARATORS, _useThousandSeps);
	
	refreshAll();
}
