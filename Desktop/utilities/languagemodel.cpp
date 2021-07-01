#include "languagemodel.h"
#include "log.h"
#include "utilities/settings.h"
#include <QDebug>
#include <QDir>
#include "dirs.h"
#include "qutils.h"
#include <QDirIterator>
#include "results/resultsjsinterface.h"
#include "modules/dynamicmodule.h"

LanguageModel * LanguageModel::_singleton = nullptr;
QLocale LanguageModel::_defaultLocale = QLocale(QLocale::English, QLocale::UnitedKingdom);

LanguageModel::LanguageInfo::LanguageInfo(const QLocale& _locale, const QString& _qmFilename)
	 : locale(_locale)
{
	if (!_qmFilename.isEmpty()) qmFilenames.push_back(_qmFilename);
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
	QString defaultLanguageName = languageName(_defaultLocale);
	_languages[defaultLanguageName] = LanguageInfo(_defaultLocale);

	findQmFiles();

	QLocale::Language prefLanguage = static_cast<QLocale::Language>(Settings::value(Settings::PREFERRED_LANGUAGE).toInt());
	QLocale::Country prefCountry = static_cast<QLocale::Country>(Settings::value(Settings::PREFERRED_COUNTRY).toInt());
	_currentLanguage = languageName(QLocale(prefLanguage, prefCountry));
	if (!_languages.contains(_currentLanguage)) _currentLanguage = defaultLanguageName;

	if (_currentLanguage != defaultLanguageName)
	{
		// Load all translated language files for specific language
		loadQmFilesForLanguage(_currentLanguage);
		_qml->retranslate();
	}

}

QVariant LanguageModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	QString languageName = _languages.keys()[index.row()];

	QString result;
	switch(role)
	{
	case NameRole:
	case Qt::DisplayRole:
	case LabelRole:
	case ValueRole:			result = languageName; break;
	case NationFlagRole:	result = "qrc:/translations/images/flag_" + languageCode(languageName) + ".png"; break;
	case LocalNameRole:		result = languageCode(languageName); break;
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
	if (language == _currentLanguage)
		return;

	_currentLanguage = language;

	if (_currentLanguage == languageName(_defaultLocale))	removeTranslators();
	else													loadQmFilesForLanguage(_currentLanguage);

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
	Settings::setValue(Settings::PREFERRED_LANGUAGE, _languages[_currentLanguage].locale.language());
	Settings::setValue(Settings::PREFERRED_COUNTRY, _languages[_currentLanguage].locale.country());
	_shouldEmitLanguageChanged = true;
	
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

bool LanguageModel::isValidLocaleName(const QString& filename, QLocale & locale)
{
	//Checks if the filename has a proper localename suffix for a valid CLocale::Language
	//https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
	//We use the two lettecode in the qm filename

	int		start		= filename.indexOf('-');
	QString localeName	= filename.mid(start + 1);
	int		end			= localeName.lastIndexOf('.');

	locale = localeName.left(end);
	return locale != QLocale(QLocale::C);
}

void LanguageModel::loadModuleTranslationFiles(Modules::DynamicModule *dyn)
{
	Log::log() << "LanguageModel::loadModuleTranslationFile called for module: " << (dyn ? dyn->name() : "NULL") << std::endl;

	bool result;
	QLocale loc;
	bool newfileloaded = false;

	//Get qm folder as subfolder from qml folder
	QString qmFolder = QString::fromStdString(dyn->qmlFilePath("")) + "translations";

	QDirIterator qdi(qmFolder, QStringList() << "*.qm" << "*.QM");

	while (qdi.hasNext())
	{
		qdi.next();
		QFileInfo fi = qdi.fileInfo();

		//Can QLocale be found from localname suffix?
		if (!isValidLocaleName(fi.fileName(), loc))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		QString language = languageName(loc);
		if (!_languages.contains(language))
		{
			Log::log() << "Not a Jasp supported language in: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		LanguageInfo & li = _languages[language];
		QString addFile = fi.filePath();
		li.qmFilenames.push_back(addFile);
		//li.qmFilenames.push_front(addFile);

		if (language != _currentLanguage)
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

	QDirIterator qdi(_qmLocation, QStringList() << "*.qm" << "*.QM");

	if(!qdi.hasNext())
		throw std::runtime_error("Could not find *any* qm-files!");

	while (qdi.hasNext())
	{
		qdi.next();

		QFileInfo fi = qdi.fileInfo();

		Log::log() << "Checking qm file: " << fi.absoluteFilePath() << std::endl;

		if (!isValidLocaleName(fi.fileName(), loc))
		{
			Log::log() << "Invalid translation file found with name: " << fi.fileName().toStdString()  << std::endl ;
			continue;
		}

		QString language = languageName(loc);

		if (!_languages.contains(language))
		{
			Log::log() << "Language (" << loc.language() << ") not registered in LanguageModel, adding it now" << std::endl;
			_languages[language] = LanguageInfo(loc, fi.filePath());
		}
		else
		{
			Log::log() << "More translated language files for a language that was already register in LanguageModel" << std::endl;
			_languages[language].qmFilenames.push_back(fi.filePath());
		}
	}

}

void LanguageModel::loadQmFilesForLanguage(const QString& language)
{
	LanguageInfo & li = _languages[language];

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

QString LanguageModel::languageCode(const QLocale& locale) const
{
	return locale.name().split("_")[0];
}

QString LanguageModel::languageCode(const QString& languageName) const
{
	return languageCode(_languages[languageName].locale);
}


QString LanguageModel::languageName(const QLocale &loc) const
{
	return languageCode(loc) + " - " + loc.nativeLanguageName();
}
