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


LanguageModel::LanguageModel(QString qmsourcepath, QApplication *app, QObject *parent)
	: QAbstractListModel(parent)
{

	_mApp = app;
	_parent = parent;
	_qmlocation = QDir::currentPath() + QDir::separator() + qmsourcepath;

	//Support English as native JASP language
	_languages.push_back(QLocale::English);
	QLocale loc(QLocale::English);

	//loc.name();				//en_US
	//loc.nativeCountryName();	//United States
	//loc.nativeLanguageName(); //American English

	_languagesInfo[QLocale::English] = LanguageInfo ("English", loc.name(), "");

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
		bool changed = _mTransLator->load(li.qmFilename, _qmlocation);
		_mApp->installTranslator(_mTransLator);
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

void LanguageModel::updateLanguage(int index, QQuickItem* item)
{	
	// Called from PrefsAdvanced.qml

	QLocale::Language cl = _languages[index];
	LanguageInfo li = _languagesInfo[cl];

	bool changed = false;

	if (cl == QLocale::English)
	{
		_mApp->removeTranslator(_mTransLator);
		changed = true;
	}
	else
	{
		changed=_mTransLator->load(li.qmFilename, _qmlocation);
		_mApp->installTranslator(_mTransLator);
	}

	if (changed)
		Settings::setValue(Settings::PREFERRED_LANGUAGE, cl);

	//QQmlApplicationEngine engine;
	//engine.retranslate();

	//QQmlApplicationEngine *ae = new QQmlApplicationEngine(mApp);
	//ae->retranslate();

	//qmlEngine(item)->retranslate();  //Bochus

	//setCurrentIndex(index);

	//QQmlEngine *qe = new QQmlEngine();
	//qe->retranslate();

	//QQmlEngine *qe = new QQmlEngine(mApp);
	//qe->retranslate();

	//QQmlEngine *qe = new QQmlEngine(_parent);
	//qe->retranslate();

	//QQmlApplicationEngine *ae = new QQmlApplicationEngine(_parent);
	//ae->retranslate();

	//QQuickView *qv = new QQuickView(QUrl("/Users/fransmeerhoff/JASP/Develop/jasp-desktop/JASP-Desktop/components/JASP/Widgets/FileMenu/PrefsAdvanced.qml"));
	//qv->engine()->retranslate();

	emit languageChanged("");

	//QQmlApplicationEngine engine("/Users/fransmeerhoff/JASP/Develop/jasp-desktop/JASP-Desktop/components/JASP/Widgets/FileMenu/PrefsAdvanced.qml");
	//engine.retranslate();

	//QQmlApplicationEngine engine;
	//engine.retranslate();

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
		QLocale loc(localname);
		QString languagename = loc.nativeLanguageName();		
		QString qmfilename = dir.filePath(i.value());
		i.setValue(dir.filePath(i.value()));
		_languages.push_back(loc.language());
		_languagesInfo[loc.language()] = LanguageInfo(languagename, localname, filename);
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



