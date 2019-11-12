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

using namespace std;

LanguageInfo  LanguageModel::CurrentLanguageInfo;

LanguageModel::LanguageModel(QString qmsourcepath, QApplication *app, QObject *parent)
	: QAbstractListModel(parent)
{
	_mApp = app;
	_parent = parent;

	_qmlocation = QDir::currentPath() + QDir::separator() + qmsourcepath;

	//Support English as native JASP language
	_languages.push_back(QLocale::English);  // 31
	QLocale loc(QLocale::English);

	// No language file needed for English (only to show in dropdown in preferences languages)
	_languagesInfo[QLocale::English] = LanguageInfo ("English", loc.name(), "");

	//Default values are now:
	//loc.name();				//en_US
	//loc.nativeCountryName();	//United States
	//loc.nativeLanguageName(); //American English

	findQmFiles();

	QQmlEngine *e = new QQmlEngine(parent);
	_mTransLator = new QTranslator(e);

	QString qmfilename; //Name of the tranlated message file name

	int cl = Settings::value(Settings::PREFERRED_LANGUAGE).toInt();

	QString defaultLocale = QLocale::system().name(); // e.g. "nl_NL"
	defaultLocale.truncate(defaultLocale.lastIndexOf('_')); // e.g. "nl"

	QLocale::Language prefLanguage = static_cast<QLocale::Language>(Settings::value(Settings::PREFERRED_LANGUAGE).toInt());

	if ( prefLanguage == 0 ) // No preferrred language ever set
	{
		Settings::setValue(Settings::PREFERRED_LANGUAGE, QLocale::English);
		_mApp->removeTranslator(_mTransLator);
		setCurrentIndex(0);
	}
	else
	{
		LanguageInfo li = _languagesInfo[prefLanguage];
		setCurrentIndex(_languages.indexOf(prefLanguage)); //Update the PrefAdvanced info

		// Load all translated language files for specific languages
		for (QString qmfilename: li.qmFilenames)
			_mTransLator->load(qmfilename, _qmlocation);

		_mApp->installTranslator(_mTransLator);
		CurrentLanguageInfo = li;
	}

	e->retranslate();

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
	case NameRole:			result = getLocalName(cl); break;
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

	bool changed = false;
	QLocale::Language cl = _languages[index];
	LanguageInfo li = _languagesInfo[cl];

	if (cl == QLocale::English)
	{
		_mApp->removeTranslator(_mTransLator);
		changed = true;
	}
	else
	{
		for (QString qmfilename: li.qmFilenames)
			_mTransLator->load(qmfilename, _qmlocation);
		changed = _mApp->installTranslator(_mTransLator);
	}

	if (changed)
	{
		Settings::setValue(Settings::PREFERRED_LANGUAGE, cl);
		setCurrentIndex(index);
		emit languageChanged();
		MessageForwarder::showWarning(tr("After changing languages you need to restart JASP for it to take effect."));
	}
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

	//QString localname = filename.right(filename.indexOf('_', 0) + 1);
	//localname = localname.left(localname.lastIndexOf('.'));

	int start = filename.indexOf('_');
	QString localname = filename.mid(start + 1);

	int end = localname.lastIndexOf('.');
	localname = localname.left(end);

	return localname;

}

QString LanguageModel::getQmTranslationName(QLocale::Language cl) const
{

	QString qmfilename;
	QString basename = "jasp_";

	LanguageInfo li = _languagesInfo[cl];

	qmfilename  = basename + li.localName + ".qm";

	return qmfilename;
}

void LanguageModel::setCurrentIndex(int currentIndex)
{
	if (m_currentIndex == currentIndex)
		return;

	m_currentIndex = currentIndex;
	emit currentIndexChanged(m_currentIndex);
}

QStringList LanguageModel::findQmFiles()
{	

	QDir dir(_qmlocation);

	QStringList fileNames = dir.entryList(QStringList("*.qm"), QDir::Files, QDir::Name);
	QMutableStringListIterator i(fileNames);

	while (i.hasNext()) {
		i.next();
		QString filename =  i.value();
		QString localname = getLocalNameFromQmFileName(filename);
		QLocale lock(localname);
		QLocale loc(localname);
		QString languagename = "";
		languagename = loc.nativeLanguageName();
		if (languagename == "")
		{
			// Invalid tranlation file found
			Log::log() << "Invalid tranlation file found with name: " << filename.toStdString()  << std::endl ;
			continue;
		}
		QString qmfilename = dir.filePath(i.value());
		i.setValue(dir.filePath(i.value()));
		if (count(_languages.begin(), _languages.end(), loc.language()) == 0)
		{
			//First Language found
			_languages.push_back(loc.language());
			_languagesInfo[loc.language()] = LanguageInfo(languagename, localname, filename);
		}
		else
			//More translated language files for a language
			_languagesInfo[loc.language()].qmFilenames.push_back(filename);
	}
	return fileNames;
}

int LanguageModel::currentIndex() const
{
	return m_currentIndex;
}

QString LanguageModel::getEmptyString() const
{
	return tr("");
}

void LanguageModel::setApplicationEngine(QQmlApplicationEngine * ae)
{
	_qml = ae;

}



