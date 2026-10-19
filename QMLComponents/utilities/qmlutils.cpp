#include <QQmlIncubator>
#include <QQmlContext>
#include <QFileInfo>
#include <QQuickStyle>
#include "qmlutils.h"
#include "qutils.h"
#include "log.h"
#include "modules/description/description.h"
#include "modules/description/entrybase.h"
#include "modules/upgrader/upgrade.h"
#include "modules/upgrader/upgrades.h"
#include "modules/upgrader/changejs.h"
#include "modules/upgrader/changecopy.h"
#include "modules/upgrader/changeremove.h"
#include "modules/upgrader/changerename.h"
#include "modules/upgrader/changesetvalue.h"
#include "modules/upgrader/changeincompatible.h"
#include "columnencoder.h"
#include "models/term.h"
#include "jaspcontrol.h"
#include "altnavpostfixassignmentstrategy.h"
#include "messageforwarder.h"
#include "preferencesmodelbase.h"
#include "jasptheme.h"
#include "knownissues.h"
#include "workspace.h"


#ifdef linux
#include <QtGlobal>
#include <QStandardPaths>
#include "appinfo.h"
#endif


const char * qmlLoadError::what() const noexcept
{
	//Just here to have an out-of-line virtual method so that clang and gcc don't complain so much
	return std::runtime_error::what();
}


QmlUtils::QmlUtils(QObject *parent) : QObject(parent)
{

}

static ColumnEncoder * currentDatasetEncoder()
{
	//The process-global current-encoder indirection is only populated inside the engine, so the
	//desktop must en/decode against the shown dataset's own encoder instead.
	Workspace * ws = Workspace::singleton();
	DataSet * ds = ws ? ws->shownDataSet() : nullptr;

	return ds ? &ds->encoder() : nullptr;
}

QString QmlUtils::encodeAllColumnNames(const QString & str)
{
	ColumnEncoder * encoder = currentDatasetEncoder();
	return encoder ? tq(encoder->encodeAll(fq(str))) : str;
}

QString QmlUtils::decodeAllColumnNames(const QString & str)
{
	ColumnEncoder * encoder = currentDatasetEncoder();
	return encoder ? tq(encoder->decodeAll(fq(str))) : str;
}

QJSValue	QmlUtils::encodeJson(const QJSValue	& val, QQuickItem * caller)
{
	Json::Value v(fqj(val));
	if(ColumnEncoder * encoder = currentDatasetEncoder())
		encoder->encodeJson(v);
	return tqj(v, caller);
}

QJSValue	QmlUtils::decodeJson(const QJSValue	& val, QQuickItem * caller)
{
	Json::Value v(fqj(val));
	if(ColumnEncoder * encoder = currentDatasetEncoder())
		encoder->decodeJson(v);
	return tqj(v, caller);
}

double QmlUtils::variantToDouble(const QVariant &val)
{
	double dblVal;
	
	if(QColumnUtils::getDoubleValue(val, dblVal))
		return dblVal;
	
	return NAN;
}

int QmlUtils::variantToInt(const QVariant &val)
{
	int intVal;
	
	if(QColumnUtils::getIntValue(val, intVal))
		return intVal;
	
	return 0;
}

//Turning QMLENGINE_DOES_ALL_THE_WORK on also works fine, but has slightly less transparent errormsgs so isn't recommended
//#define QMLENGINE_DOES_ALL_THE_WORK

QObject * instantiateQml(const QString & qmlTxt, const QUrl & url, const std::string & moduleName, const std::string & whatAmILoading, const std::string & filename, QQmlContext * ctxt)
{
	QObject * obj = nullptr;

#ifdef QMLENGINE_DOES_ALL_THE_WORK
	obj = MainWindow::singleton()->loadQmlData(qmlTxt, url);
#else
//	if(!ctxt)
//		ctxt = MainWindow::singleton()->giveRootQmlContext();

	QQmlComponent qmlComp(ctxt->engine());

	//Log::log() << "Setting url to '" << url.toString() << "' for Description.qml.\n" << std::endl;// data: '" << descriptionTxt << "'\n"<< std::endl;

	qmlComp.setData(qmlTxt.toUtf8(), url);

	if(qmlComp.isLoading())
		Log::log() << whatAmILoading << " for module " << moduleName << " is still loading, make sure you load a local file and that Windows doesn't mess this up for you..." << std::endl;


	auto errorLogger =[&](bool isError, QList<QQmlError> errors)
	{
		if(!isError) return;

		std::stringstream out;

		out << "Loading " << filename << " for module " << moduleName << " had errors:\n";

		for(const QQmlError & error : errors)
			out << error.toString() << "\n";

		Log::log() << out.str() << std::flush;

		throw qmlLoadError("There were errors loading " + filename + ":\n" + out.str());
	};

	errorLogger(qmlComp.isError(), qmlComp.errors());

	if(!qmlComp.isReady())
		throw qmlLoadError(whatAmILoading + " Component is not ready!");

	QQmlIncubator localIncubator(QQmlIncubator::Synchronous);


	qmlComp.create(localIncubator);

	errorLogger(localIncubator.isError(), localIncubator.errors());

	obj = localIncubator.object();

#endif

	return obj;
}

QObject * instantiateQml(const QUrl & filePath, const std::string & moduleName, QQmlContext * ctxt)
{
	if(!filePath.isLocalFile())
		throw std::runtime_error(fq(filePath.toLocalFile()) + " is not a local file...");

	QFileInfo	qmlFileInfo(filePath.toLocalFile());

	if(!qmlFileInfo.exists())
		throw std::runtime_error(fq(qmlFileInfo.absoluteFilePath()) + " does not exist...");

	QString 	qmlTxt;
	QFile		qmlFile(qmlFileInfo.absoluteFilePath());

	if(!qmlFile.open(QIODevice::ReadOnly))
		Log::log() << "Cannot open qmlFile: " << qmlFile.fileName() << " with error: " << qmlFile.errorString() << std::endl;
	qmlTxt =	qmlFile.readAll();
				qmlFile.close();

	return instantiateQml(qmlTxt, filePath, moduleName, fq(qmlFileInfo.absoluteFilePath()), fq(qmlFileInfo.fileName()),  ctxt);

}


#ifdef linux

void QmlUtils::configureQMLCacheDir() 
{
	//set cache environment variable
	QDir cacheDir = QmlUtils::generateQMLCacheDir();
	bool set = qputenv("QML_DISK_CACHE_PATH", cacheDir.absolutePath().toLocal8Bit());
	if(!set)
		throw std::runtime_error("Could not set qml cache directory in environment");

	//delete stale caches
	QDir parent = cacheDir;
	parent.cdUp();
	QStringList staleCaches = parent.entryList(QStringList() << "qmlcache*", QDir::NoDot | QDir::NoDotDot | QDir::Dirs);

	for(auto& cacheName: staleCaches) 
	{
		QDir staleCache = parent;
		staleCache.cd(cacheName);
		if(cacheDir.absolutePath() != staleCache.absolutePath())
			staleCache.removeRecursively();
	}
	Log::log() << "QML cache directory: " + cacheDir.absolutePath() << std::endl;
}

QDir QmlUtils::generateQMLCacheDir() 
{
	QString commit 		= tq(AppInfo::gitCommit),
	 		basePath 	= qEnvironmentVariable("QML_DISK_CACHE_PATH", QStandardPaths::writableLocation(QStandardPaths::CacheLocation)),
	 		path 		= basePath + "/qmlcache_" + commit;
	QDir cacheDir(path);

	if(!cacheDir.exists() && !cacheDir.mkpath("."))
		throw std::runtime_error("Could not create qml cache directory: " + fq(cacheDir.absolutePath()));
		
	cacheDir.refresh();
	return cacheDir;
}

#endif

void QmlUtils::setGlobalPropertiesInQMLContext(QQmlContext * ctxt)
{
	bool	debug		= false,
			isMac		= false,
			isLinux		= false,
			buildingPro	= false,
			interactive	= false;

#ifdef JASP_DEBUG
	debug = true;
#endif

#ifdef __APPLE__
	isMac = true;
#endif

#ifdef __linux__
	isLinux = true;
#endif

	bool isWindows = !isMac && !isLinux;
	
		
#ifdef PRO
		buildingPro = true;
#endif
	
#ifdef INTERACTIVE_PLOTS
		interactive = true;
#endif
	

	ctxt->setContextProperty("PRO",						buildingPro);
	ctxt->setContextProperty("MACOS",					isMac);
	ctxt->setContextProperty("LINUX",					isLinux);
	ctxt->setContextProperty("WINDOWS",					isWindows);
	ctxt->setContextProperty("DEBUG_MODE",				debug);
	ctxt->setContextProperty("INTERACTIVE_PLOTS",		interactive);
	ctxt->setContextProperty("INTERACTION_SEPARATOR",	Term::separator);
	
	ctxt->setContextProperty("dataSetInfo",				Workspace::singleton() ? Workspace::singleton()->varInfo() : nullptr);
	ctxt->setContextProperty("messages",				MessageForwarder::msgForwarder());
	ctxt->setContextProperty("backgroundForms",			nullptr);


	qmlRegisterUncreatableType<JASPControl>(					"JASP",		1, 0, "JASP",					"Impossible to create JASP Object");
	qmlRegisterUncreatableType<ALTNavPostfixAssignmentStrategy>("JASP",		1, 0, "AssignmentStrategy",		"Can't make it"	);
}

void QmlUtils::registerQmlModuleTypes()
{
	qmlRegisterType<Modules::Description>						("JASP.Module", 1, 0, "Description"						);
	qmlRegisterType<Modules::AnalysisItem>						("JASP.Module", 1, 0, "Analysis"						);
	qmlRegisterType<Modules::Separator>							("JASP.Module", 1, 0, "Separator"						);
	qmlRegisterType<Modules::GroupTitle>						("JASP.Module", 1, 0, "GroupTitle"						);
	qmlRegisterType<Modules::GroupTitleSmall>					("JASP.Module", 1, 0, "GroupTitleSmall"					);
	qmlRegisterType<Modules::Upgrades>							("JASP.Module", 1, 0, "Upgrades"						);
	qmlRegisterType<Modules::Upgrade>							("JASP.Module", 1, 0, "Upgrade"							);
	qmlRegisterType<Modules::ChangeJS>							("JASP.Module", 1, 0, "ChangeJS"						);
	qmlRegisterType<Modules::ChangeCopy>						("JASP.Module", 1, 0, "ChangeCopy"						);
	qmlRegisterType<Modules::ChangeRename>						("JASP.Module", 1, 0, "ChangeRename"					);
	qmlRegisterType<Modules::ChangeRemove>						("JASP.Module", 1, 0, "ChangeRemove"					);
	qmlRegisterType<Modules::ChangeIncompatible>				("JASP.Module", 1, 0, "ChangeIncompatible"				);
	qmlRegisterType<Modules::ChangeSetValue>					("JASP.Module", 1, 0, "ChangeSetValue"					);
	qmlRegisterUncreatableType<Modules::EntryBase>				("JASP.Module", 1, 0, "EntryBase",						"Superclass for menu entries, shouldn't be instantiated manually");
	qmlRegisterUncreatableType<Modules::DynamicModule>			("JASP.Module", 1, 0, "DynamicModule",					"Can only be instantiated by JASP");
	qmlRegisterUncreatableType<Modules::DescriptionChildBase>	("JASP.Module", 1, 0, "DescriptionChildBase",			"Superclass for Description info, shouldn't be instantiated manually");
}

void QmlUtils::setupQMLEngine(QQmlEngine *engine)
{
	QmlUtils::setGlobalPropertiesInQMLContext(engine->rootContext());

	PreferencesModelBase* prefModel = engine->rootContext()->contextProperty("preferencesModel").value<PreferencesModelBase*>();
	if (prefModel == nullptr)
	{
		prefModel = PreferencesModelBase::preferences();
		if (!prefModel)
			prefModel = new PreferencesModelBase();
		engine->rootContext()->setContextProperty("preferencesModel",		prefModel);
	}

	if (engine->rootContext()->contextProperty("jaspTheme").isNull())
	{
		JaspTheme* defaultJaspTheme = JaspTheme::currentTheme();
		if (!defaultJaspTheme)
		{
			defaultJaspTheme = new JaspTheme();
			defaultJaspTheme->setThemeName("lightTheme");
		}
		engine->rootContext()->setContextProperty("jaspTheme",			defaultJaspTheme);
	}

	if (!KnownIssues::issues())
		new KnownIssues();

	QStringList originalImportPaths = engine->importPathList();
	if (!originalImportPaths.contains(":/jasp-stats.org/imports"))
		engine->addImportPath(":/jasp-stats.org/imports");

	engine->rootContext()->setContextProperty("NO_DESKTOP_MODE",	true);

	static bool alreadyDone = false;
	if (!alreadyDone)
	{
		qmlRegisterUncreatableMetaObject(JASPControl::staticMetaObject, // static meta object
										 "JASP.Controls",        // import statement
										 0, 1,                   // major and minor version of the import
										 "JASP",                 // name in QML
										 "Error: only enums");

		QQuickStyle::setStyle("Basic"); // This removes warnings "The current style does not support customization of this control"
		alreadyDone = true;
	}

}
