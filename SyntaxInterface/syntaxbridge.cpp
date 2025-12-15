//
// Copyright (C) 2013-2025 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "syntaxbridge_interface.h"
#include "syntaxbridge.h"
#include <QQmlApplicationEngine>
#include <QQmlContext>
#include <QFileInfo>
#include <QQmlComponent>
#include <QQuickItem>
#include <QDir>
#include <QThread>
#include <QQmlIncubator>
#include "controls/jaspcontrol.h"
#include "datasetprovider.h"
#include "databridge.h"
#include "rbridge.h"
#include "processinfo.h"
#include "tempfiles.h"
#include "analysisbase.h"
#include "analysisform.h"
#include "log.h"
#include "utilities/qmlutils.h"
#include "dirs.h"
#include "utilities/appdirs.h"


#include <QtPlugin>
#ifdef USE_QT_STATIC_LIBS
Q_IMPORT_PLUGIN(QMinimalIntegrationPlugin)
#endif
Q_IMPORT_PLUGIN(JASP_ControlsPlugin)

#define _STRINGIZE(x) #x
#define STRINGIZE(x) _STRINGIZE(x)

static bool									gl_initialized					= false;
static QGuiApplication			*			gl_application					= nullptr;
static QQmlEngine				*			gl_qmlEngine					= nullptr;
static DataBridge				*			gl_dataBridge					= nullptr;
static ColumnEncoder			*			gl_extraEncodings				= nullptr;
static QMap<QString, std::pair<QDateTime, AnalysisForm* > >	gl_qmlFormMap;

static bool									gl_verbose						=
#ifdef JASP_DEBUG
	true;
#else
	false;
#endif

static std::string							gl_param_resultFont				=
#ifdef WIN32
	"Arial,sans-serif,freesans,\"Segoe UI\"";
#elif __APPLE__
	"\"Lucida Grande\",Helvetica,Arial,sans-serif,\"Helvetica Neue\",freesans";
#else
	"freesans,sans-serif";
#endif

extern "C" {
void STDCALL syntaxBridgeCleanup()
{
	for (auto value : gl_qmlFormMap.values())
		deleteQuickItem(value.second);

	gl_qmlFormMap.clear();

	if (gl_qmlEngine)
	{
		gl_qmlEngine->clearSingletons();
		gl_qmlEngine->clearComponentCache();
	}
}

void STDCALL syntaxBridgeLoadDataSet(const SyntaxBridgeDataSet* syntaxBridgeDataSet, bool dbInMemory, int threshold, bool orderLabelsByValue)
{
	if (!init(dbInMemory))
	{
		Log::log() << "Error during initialization" << std::endl;
		return;
	}

	DataSetProvider* provider = DataSetProvider::getProvider(dbInMemory);

	std::map<std::string, stringvec > dataSet;

	for (int colNr = 0; colNr < syntaxBridgeDataSet->columnCount; colNr++)
	{
		const SyntaxBridgeColumn& column  = syntaxBridgeDataSet->columns[colNr];
		stringvec values;
		for (int rowNr = 0; rowNr < syntaxBridgeDataSet->rowCount; rowNr++)
			values.push_back(column.values[rowNr]);
		dataSet[column.name] = values;
	}

	provider->loadDataSet(dataSet, threshold, orderLabelsByValue);
}

const char* STDCALL syntaxBridgeLoadQmlAndParseOptions(const char* moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData)
{
	if (!init())
	{
		Log::log() << "Error during initialization" << std::endl;
		return "";
	}

	std::string qmlFileStr		= qmlFile,
				versionStr		= version,
				analysisNameStr	= analysisName,
				moduleNameStr	= moduleName;


	AnalysisForm* form = getQmlForm(tq(qmlFileStr));

	if (!form)
	{
		Log::log() << "Cannot create QML Form " << qmlFileStr << std::endl;
		return "";
	}

	Json::Value parsedOptions;
	std::string errorMsg;

	if (!form->parseOptions(options, parsedOptions, errorMsg))
	{
		Log::log() << "Error when parsing options: " << errorMsg << std::endl;
		return "";
	}

	gl_extraEncodings->setCurrentNamesFromOptionsMeta(parsedOptions);
	gl_dataBridge->updateOptionsAccordingToMeta(parsedOptions);
	ColumnEncoder::colsPlusTypes analysisColsTypes = ColumnEncoder::encodeColumnNamesinOptions(parsedOptions, preloadData);

	rbridge_setWantedCols(analysisColsTypes);

	static std::string result;
	result = parsedOptions.toStyledString();

	return result.c_str();
}


const char* STDCALL syntaxBridgeGenerateModuleWrappers(const char* modulePath, bool preloadData)
{
	if (!init())
		return "Error during initialization";

	static std::string result;
	QString modulePathQ		= tq(modulePath),
			moduleNameQ;

	QDir moduleDir(modulePathQ);

	if (!moduleDir.exists())
	{
		result = fq("Module path not found: " + modulePathQ);
		return result.c_str();
	}

	QVector<AnalysisInfo> analyses;
	QFile qmlDescriptionFile(modulePathQ + "/inst/Description.qml");
	if (!qmlDescriptionFile.exists())
	{
		result = "Description.qml file not found in " + fq(modulePathQ);
		return result.c_str();
	}

	QString fileContent;
	QStringList analysesPart;
	// TODO: The Description.qml cannot be loaded by the QML Engine, since it does not have access to the JASP.Module
	// If JASP.Module is set as a a Qt module/plugin (as JASP.Controls), then we could load it and uses the Description object directly.
	// For the time being, just try to parse the Description.qml to detect the Analyses and their properties.
	if (qmlDescriptionFile.open(QIODevice::ReadOnly))
	{
		fileContent = qmlDescriptionFile.readAll();
		analysesPart = fileContent.split("Analysis");
		for (int i = 1; i < analysesPart.length(); i++)
		{
			QStringList lines = analysesPart[i].split("\n");
			QString analysisName, qmlFileName, analysisTitle;

			for (QString line : lines)
			{
				line = line.trimmed();
				if (line.startsWith("func"))
					analysisName = line.split(":")[1].trimmed().replace('"', "");
				else if (line.startsWith("qml"))
					qmlFileName = line.split(":")[1].trimmed().replace('"', "");
				else if (line.startsWith("title"))
				{
					analysisTitle = line.split(":")[1].trimmed();
					if (analysisTitle.startsWith("qsTr"))
						analysisTitle = analysisTitle.remove(0, std::string("qsTr(\"").length()).chopped(1);

					analysisTitle = analysisTitle.replace('"', "");
				}
			}

			if (!analysisName.isEmpty())
			{
				if (qmlFileName.isEmpty())
					qmlFileName = analysisName + ".qml";
				analyses.append(AnalysisInfo(analysisName, qmlFileName, analysisTitle));
			}
		}
	}

	for (auto & analysis : analyses)
	{
		Log::log() << "Analysis " << fq(analysis.analysisName) << " with qml file " << fq(analysis.qmlFileName) << std::endl;
		if (!generateWrapper(modulePathQ, analysis.analysisName, analysis.qmlFileName, analysis.analysisTitle, preloadData))
		{
			result = "Error when generating wrapper of " + fq(analysis.analysisName);
			return result.c_str();
		}
	}

	return "Wrappers generated";
}


const char* STDCALL syntaxBridgeGenerateAnalysisWrapper(const char* modulePath, const char* qmlFileName, const char* analysisName, const char* analysisTitle, bool preloadData)
{
	if (!init())
		return "Error during initialization";

	static std::string result;

	gl_qmlEngine->clearComponentCache();

	QString qmlFileNameQ	= tq(qmlFileName),
			modulePathQ		= tq(modulePath),
			moduleNameQ,
			analysisNameQ	= tq(analysisName),
			analysisTitleQ	= tq(analysisTitle);

	QDir moduleDir(modulePathQ);

	if (!moduleDir.exists())
	{
		result = "Module path not found: " + fq(modulePathQ);
		return result.c_str();
	}

	// If JASP.Module is set as a a Qt module/plugin (as JASP.Controls), then we could load it and uses the Description object directly, and know directly
	// what is the name of the qml file and if it uses preloadData
	if (!generateWrapper(modulePathQ, analysisNameQ, qmlFileNameQ, analysisTitleQ, preloadData))
	{
		result = "Error when generating wrapper of " + fq(analysisNameQ);
		return result.c_str();
	}

	result = "Wrapper generated for analysis " + fq(analysisNameQ);
	return result.c_str();
}

} // extern "C"


void blockSignalsRecursive(QObject* item)
{
	for (QObject* obj : item->children())
		blockSignalsRecursive(obj);
	item->blockSignals(true);
}

void deleteQuickItem(QQuickItem* item)
{
	blockSignalsRecursive(item);
	item->setParent(nullptr);
	item->setParentItem(nullptr);
	item->deleteLater();
}

void sendMessage(const char * msg)
{
	if (gl_verbose)
		Log::log() << "Send Message: " << msg << std::endl;
}

bool init(bool dbInMemory)
{
	if (gl_initialized) return true;
	gl_initialized = true;

	if (gl_verbose)
	{
		QString qt_install_dir = qgetenv("QT_DIR");
#ifdef QT_DIR
		if (qt_install_dir.isEmpty())
			qt_install_dir = STRINGIZE(QT_DIR);
#endif
		Log::log() << "QT_DIR found in environment: " + fq(qt_install_dir) << std::endl;

		QString rHome = qgetenv("R_HOME");
		Log::log() << "R_HOME: " << fq(rHome) << std::endl;
	}

	int					dummyArgc = 1;
	char				dummyArgv[2];
	dummyArgv[0] = '?';
	dummyArgv[1] = '\0';

	//const char*	platformArg = "-platform";
	//const char*	platformOpt = "minimal"; //"cocoa";

	std::vector<const char*> arguments = {"JASP"}; //{qmlR, platformArg, platformOpt};


	int		argc = arguments.size();
	char** argvs = new char*[argc];

	for (int i = 0; i < argc; i++)
	{
		argvs[i] = new char[strlen(arguments[i]) + 1];
		memset(argvs[i], '\0',				strlen(arguments[i]) + 1);
		memcpy(argvs[i], arguments[i],		strlen(arguments[i]));
		argvs[i][							strlen(arguments[i])] = '\0';
	}

	qputenv("QT_QPA_PLATFORM", "minimal");

	gl_application = new QGuiApplication(argc, argvs);
	gl_qmlEngine = new QQmlEngine();

	Dirs::setLocalAppdataDir(AppDirs::appData(false).toStdString());
	TempFiles::init(ProcessInfo::currentPID());
	DataSetProvider::getProvider(dbInMemory, false, gl_application); // Create the DataSetProvider in case the loadDataSet was not already called

	QmlUtils::setupQMLEngine(gl_qmlEngine);

	gl_dataBridge = new DataBridge(ProcessInfo::currentPID(), dbInMemory);
	gl_extraEncodings = new ColumnEncoder("JaspExtraOptions_");

	rbridge_init(gl_dataBridge, sendMessage, [](){ return false; }, gl_extraEncodings, gl_param_resultFont.c_str(), false);

	jaspRCPP_init_jaspBase();

	return true;
}

void sendRScriptHandler(AnalysisForm* form, QString script, QString controlName, bool whiteListedVersion)
{
	if (gl_verbose)
		Log::log() << "R Script " << fq(script) << " sent by " << controlName << std::endl;

	bool hasError = false;
	std::string result = rbridge_evalRCodeWhiteListed(fq(script).c_str(), whiteListedVersion);

	if (result == "")
	{
		hasError = true;
		result = jaspRCPP_getLastErrorMsg();
	}

	if (gl_verbose)
		Log::log() << "R Script result " << (hasError ? "has error" : "") << ": " << result << std::endl;

	form->runScriptRequestDone(tq(result), controlName, hasError);
}

AnalysisForm* getQmlForm(const QString& qmlFileStr)
{
	AnalysisForm* qmlForm = nullptr;

	QFileInfo	qmlFileInfo(qmlFileStr);
	if (!qmlFileInfo.exists())
	{
		Log::log() << "File not found: " << fq(qmlFileStr) << std::endl;
		return nullptr;
	}

	if (gl_qmlFormMap.contains(qmlFileStr) && gl_qmlFormMap[qmlFileStr].first == qmlFileInfo.lastModified())
		qmlForm = gl_qmlFormMap[qmlFileStr].second;
	else
	{
		QUrl urlFile = QUrl::fromLocalFile(qmlFileInfo.absoluteFilePath());

		QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
		QQmlComponent qmlComp( gl_qmlEngine, urlFile, QQmlComponent::PreferSynchronous);
		QQmlContext* context = gl_qmlEngine->rootContext();

		qmlComp.create(localIncubator, context);

		switch (localIncubator.status())
		{
		case QQmlIncubator::Null:
		case QQmlIncubator::Loading:
		{
			Log::log() << "Could not load QML component!" << std::endl;
			// Try it with QQmlComponent::create: this gives a better error
			qmlForm = qobject_cast<AnalysisForm*>(qmlComp.create(context));
			if (qmlForm)
				Log::log() << "Form could be loaded via QQmlComponent::create instead of QQmlIncubator::created. Quite weird!" << std::endl;
			else
			{
				for(const auto & error : qmlComp.errors())
					Log::log() << "Error when creating component at " << fq(QString::number(error.line())) << "," << fq(QString::number(error.column())) << " in file " << error.url().toString() << ": " << fq(error.description()) << std::endl;

				return nullptr;
			}
		}
		case QQmlIncubator::Error:
			Log::log() << "Error when creating component!" << std::endl;
			for(const auto & error : localIncubator.errors())
				Log::log() << "Error when creating component at " << fq(QString::number(error.line())) << "," << fq(QString::number(error.column())) << " in file " << error.url().toString() << ": " << fq(error.description()) << std::endl;
			return nullptr;
		case QQmlIncubator::Ready:
		{
			Log::log() << "QML form created" << std::endl;
			qmlForm = qobject_cast<AnalysisForm*>(localIncubator.object());
			if (!qmlForm)
			{
				Log::log() << "Object created is not an AnalysisForm object!!" << std::endl;
				return nullptr;
			}

			break;
		}
		}


		AnalysisBase* analysis = qmlForm->analysisObj();
		if (!analysis)
		{
			analysis = new AnalysisBase(qmlForm); // Make dummy analysis
			qmlForm->setAnalysis(analysis);
		}

		QObject::connect(analysis,	&AnalysisBase::sendRScriptSignal,	[qmlForm](QString script, QString controlName, bool whiteListedVersion, QString module) { sendRScriptHandler(qmlForm, script, controlName, whiteListedVersion); });

		if (gl_qmlFormMap.contains(qmlFileStr))
			deleteQuickItem(gl_qmlFormMap[qmlFileStr].second); // delete old version of the form
		gl_qmlFormMap[qmlFileStr] = std::make_pair(qmlFileInfo.lastModified(), qmlForm);

		gl_application->processEvents();

	}

	return qmlForm;
}


bool generateWrapper(const QString& modulePath, const QString& analysisName, const QString& qmlFileName, const QString& analysisTitle, bool preloadData)
{
	QString qmlFilePath = modulePath + "/inst/qml/" + qmlFileName;

	AnalysisForm* form = getQmlForm(qmlFilePath);
	if (!form)
	{
		Log::log() << "Cannot create the QML form " << qmlFilePath << std::endl;
		return false;
	}

	QString returnedValue = form->generateWrapper(QDir(modulePath).dirName(), analysisName, qmlFileName, analysisTitle, preloadData);

	QFile file(modulePath + "/R/" + analysisName + "Wrapper.R");
	file.resize(0); // Empty the file
	if (file.open(QIODevice::ReadWrite)) {
		QTextStream stream(&file);
		stream << returnedValue;
	}

	return true;
}








