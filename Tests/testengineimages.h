#ifndef TESTENGINEIMAGES_H
#define TESTENGINEIMAGES_H

#include <QTest>
#include <functional>
#include <json/json.h>

class EngineRepresentation;
class DataSetPackage;
class EngineSync;
class Importer;
class IPCChannel;
class PreferencesModel;
class LanguageModel;

///Image requests (resize, save, rewrite) sent to engines that never ran an analysis, as happens right after loading a .jasp file.
///These need jaspBase and its dependencies, taken from the installed jaspDescriptives module (or JASP_TEST_MODULE_LIB), and are skipped without them.
class TestEngineImages: public QObject
{
	Q_OBJECT
private slots:
	void	init();
	void	cleanup();
	void	testResizeOnFreshEngineDecodesColumnNames();

private:
	IPCChannel		*	channelOf(EngineRepresentation * engine);
	bool				loadJaspBase(EngineRepresentation * engine, const QString & moduleLib);
	Json::Value			waitForReply(EngineRepresentation * engine, std::function<bool(const Json::Value &)> isTheOne, int timeoutMs);

	EngineSync				*	_engines		= nullptr;
	EngineRepresentation	*	_analysisEngine	= nullptr,
							*	_freshEngine	= nullptr;
	DataSetPackage			*	_pkg			= nullptr;
	Importer				*	_importer		= nullptr;
	PreferencesModel		*	_prefs			= nullptr;
	LanguageModel			*	_language		= nullptr;
};

#endif // TESTENGINEIMAGES_H
