#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include <QObject>
#include <QFont>
#include "column.h"
#include "utilities/qutils.h"

class JaspTheme;

class PreferencesModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY(bool			fixedDecimals			READ fixedDecimals				WRITE setFixedDecimals				NOTIFY fixedDecimalsChanged				)
	Q_PROPERTY(int			numDecimals				READ numDecimals				WRITE setNumDecimals				NOTIFY numDecimalsChanged				)
	Q_PROPERTY(bool			exactPValues			READ exactPValues				WRITE setExactPValues				NOTIFY exactPValuesChanged				)
	Q_PROPERTY(bool			dataAutoSynchronization	READ dataAutoSynchronization	WRITE setDataAutoSynchronization	NOTIFY dataAutoSynchronizationChanged	)
	Q_PROPERTY(bool			useDefaultEditor		READ useDefaultEditor			WRITE setUseDefaultEditor			NOTIFY useDefaultEditorChanged			)
	Q_PROPERTY(QString		customEditor			READ customEditor				WRITE setCustomEditor				NOTIFY customEditorChanged				)
	Q_PROPERTY(bool			useDefaultPPI			READ useDefaultPPI				WRITE setUseDefaultPPI				NOTIFY useDefaultPPIChanged				)
	Q_PROPERTY(int			customPPI				READ customPPI					WRITE setCustomPPI					NOTIFY customPPIChanged					)
	Q_PROPERTY(bool			whiteBackground			READ whiteBackground			WRITE setWhiteBackground			NOTIFY whiteBackgroundChanged			)
	Q_PROPERTY(QString		plotBackground			READ plotBackground				WRITE setPlotBackground				NOTIFY plotBackgroundChanged			)
	Q_PROPERTY(double		uiScale					READ uiScale					WRITE setUiScale					NOTIFY uiScaleChanged					)
	Q_PROPERTY(QStringList	missingValues			READ missingValues													NOTIFY missingValuesChanged				)
	Q_PROPERTY(int			defaultPPI				READ defaultPPI					WRITE setDefaultPPI					NOTIFY defaultPPIChanged				)
	Q_PROPERTY(bool			developerMode			READ developerMode				WRITE setDeveloperMode				NOTIFY developerModeChanged				)
	Q_PROPERTY(QString		developerFolder			READ developerFolder			WRITE setDeveloperFolder			NOTIFY developerFolderChanged			)
	Q_PROPERTY(bool			customThresholdScale	READ customThresholdScale		WRITE setCustomThresholdScale		NOTIFY customThresholdScaleChanged		)
	Q_PROPERTY(int			thresholdScale			READ thresholdScale				WRITE setThresholdScale				NOTIFY thresholdScaleChanged			)
	Q_PROPERTY(bool			devModRegenDESC			READ devModRegenDESC			WRITE setDevModRegenDESC			NOTIFY devModRegenDESCChanged			)
	Q_PROPERTY(bool			logToFile				READ logToFile					WRITE setLogToFile					NOTIFY logToFileChanged					)
	Q_PROPERTY(int			logFilesMax				READ logFilesMax				WRITE setLogFilesMax				NOTIFY logFilesMaxChanged				)
	Q_PROPERTY(int			maxFlickVelocity		READ maxFlickVelocity			WRITE setMaxFlickVelocity			NOTIFY maxFlickVelocityChanged			)
	Q_PROPERTY(bool			modulesRemember			READ modulesRemember			WRITE setModulesRemember			NOTIFY modulesRememberChanged			)
	Q_PROPERTY(QStringList	modulesRemembered		READ modulesRemembered			WRITE setModulesRemembered			NOTIFY modulesRememberedChanged			)
	Q_PROPERTY(bool			safeGraphics			READ safeGraphics				WRITE setSafeGraphics				NOTIFY safeGraphicsChanged				)
	Q_PROPERTY(QString		cranRepoURL				READ cranRepoURL				WRITE setCranRepoURL				NOTIFY cranRepoURLChanged				)
	Q_PROPERTY(int			plotPPI					READ plotPPI														NOTIFY plotPPIPropChanged				)
	Q_PROPERTY(bool			useDefaultInterfaceFont	READ useDefaultInterfaceFont	WRITE setUseDefaultInterfaceFont	NOTIFY useDefaultInterfaceFontChanged	)
	Q_PROPERTY(QString		realInterfaceFont		READ realInterfaceFont												NOTIFY realInterfaceFontChanged			)
	Q_PROPERTY(QString		interfaceFont			READ interfaceFont				WRITE setInterfaceFont				NOTIFY interfaceFontChanged				)
	Q_PROPERTY(bool			useDefaultConsoleFont	READ useDefaultConsoleFont		WRITE setUseDefaultConsoleFont		NOTIFY useDefaultConsoleFontChanged		)
	Q_PROPERTY(QString		realConsoleFont			READ realConsoleFont												NOTIFY realConsoleFontChanged			)
	Q_PROPERTY(QString		consoleFont				READ consoleFont				WRITE setConsoleFont				NOTIFY consoleFontChanged				)
	Q_PROPERTY(bool			useDefaultResultFont	READ useDefaultResultFont		WRITE setUseDefaultResultFont		NOTIFY useDefaultResultFontChanged		)
	Q_PROPERTY(QString		defaultResultFont		READ defaultResultFont			CONSTANT																	)
	Q_PROPERTY(QString		realResultFont			READ realResultFont													NOTIFY realResultFontChanged			)
	Q_PROPERTY(QString		resultFont				READ resultFont					WRITE setResultFont					NOTIFY resultFontChanged				)
	Q_PROPERTY(QString		currentThemeName		READ currentThemeName			WRITE setCurrentThemeName			NOTIFY currentThemeNameChanged			)
	Q_PROPERTY(QString		languageCode			READ languageCode													NOTIFY languageCodeChanged				)
	Q_PROPERTY(bool			useNativeFileDialog		READ useNativeFileDialog		WRITE setUseNativeFileDialog		NOTIFY useNativeFileDialogChanged		)
	Q_PROPERTY(bool			disableAnimations		READ disableAnimations			WRITE setDisableAnimations			NOTIFY disableAnimationsChanged			)
	Q_PROPERTY(bool			animationsOn			READ animationsOn													NOTIFY animationsOnChanged				)
	Q_PROPERTY(bool			generateMarkdown		READ generateMarkdown			WRITE setGenerateMarkdown			NOTIFY generateMarkdownChanged			)
	Q_PROPERTY(QStringList	allFonts				READ allFonts														NOTIFY allFontsChanged					)

public:
	static PreferencesModel * prefs() { return _singleton; }


	explicit	 PreferencesModel(QObject *parent = 0);
	~PreferencesModel() { _singleton = nullptr;}

	int			customPPI()					const;
	int			numDecimals()				const;
	int			defaultPPI()				const	{ return _defaultPPI; }
	int			plotPPI()					const	{ return useDefaultPPI() ? defaultPPI() : customPPI();	}
	bool		fixedDecimals()				const;
	bool		exactPValues()				const;
	bool		dataAutoSynchronization()	const;
	bool		useDefaultEditor()			const;
	bool		useDefaultPPI()				const;
	bool		whiteBackground()			const;
	QString		plotBackground()			const;
	bool		developerMode()				const;
	double		uiScale()						 ;
	QString		customEditor()				const;
	QString		developerFolder()			const;
	QString		fixedDecimalsForJS()		const;
	QStringList	missingValues()				const;
	bool		customThresholdScale()		const;
	int			thresholdScale()			const;
	bool		devModRegenDESC()			const;
	bool		logToFile()					const;
	int			logFilesMax()				const;
	int			maxFlickVelocity()			const;
	bool		modulesRemember()			const;
	QStringList	modulesRemembered()			const;
	bool		safeGraphics()				const;
	QString		cranRepoURL()				const;
	bool		useDefaultInterfaceFont()	const;
	QString		interfaceFont()				const;
	QString		realInterfaceFont()			const;
	bool		useDefaultConsoleFont()	const;
	QString		consoleFont()				const;
	QString		realConsoleFont()			const;
	bool		useDefaultResultFont()		const;
	QString		resultFont()				const;
	QString		realResultFont()			const;
	QString		currentThemeName()			const;
	QString		languageCode()				const;
	bool		useNativeFileDialog()		const;
	bool		disableAnimations()			const;
	bool		animationsOn()				const { return !disableAnimations() && !safeGraphics(); }
	bool		generateMarkdown()			const;
	QStringList allFonts()					const { return _allFonts; }
	QString		defaultResultFont()			const;

	void		zoomIn();
	void		zoomOut();
	void		zoomReset();

public slots:
	void setUiScale(					double		uiScale);
	void setCustomPPI(					int			customPPI);
	void setDefaultPPI(					int			defaultPPI);
	void setNumDecimals(				int			numDecimals);
	void setExactPValues(				bool		exactPValues);
	void setCustomEditor(				QString		customEditor);
	void setFixedDecimals(				bool		fixedDecimals);
	void setUseDefaultPPI(				bool		useDefaultPPI);
	void setDeveloperMode(				bool		developerMode);
	void setWhiteBackground(			bool		whiteBackground);
	void setPlotBackground(				QString		plotBackground);
	void setDeveloperFolder(			QString		developerFolder);
	void setUseDefaultEditor(			bool		useDefaultEditor);
	void setDataAutoSynchronization(	bool		dataAutoSynchronization);
	void browseSpreadsheetEditor();
	void browseDeveloperFolder();
	void updateUtilsMissingValues();
	void removeMissingValue(			QString		value);
	void addMissingValue(				QString		value);
	void resetMissingValues();
	void setCustomThresholdScale(		bool		customThresholdScale);
	void setThresholdScale(				int			thresholdScale);
	void setDevModRegenDESC(			bool		devModRegenDESC);
	void setLogToFile(					bool		logToFile);
	void setLogFilesMax(				int			logFilesMax);
	void setMaxFlickVelocity(			int			maxFlickVelocity);
	void setModulesRemember(			bool		modulesRemember);
	void setModulesRemembered(			QStringList modulesRemembered);
	void setSafeGraphics(				bool		safeGraphics);
	void setCranRepoURL(				QString		cranRepoURL);
	void moduleEnabledChanged(			QString		moduleName, bool enabled);
	void onUseDefaultPPIChanged(		bool		useDefault);
	void onCustomPPIChanged(			int);
	void onDefaultPPIChanged(			int);
	void setCurrentThemeName(			QString		currentThemeName);
	void setCurrentThemeNameFromClass(	JaspTheme * theme);
	void setUseDefaultInterfaceFont(	bool		useDefaultInterfaceFont);
	void setInterfaceFont(				QString		interfaceFont);
	void setUseDefaultConsoleFont(		bool		useDefaultConsoleFont);
	void setConsoleFont(				QString		consoleFont);
	void setUseDefaultResultFont(		bool		useDefaultResultFont);
	void setResultFont(					QString		resultFont);
	void setUseNativeFileDialog(		bool		useNativeFileDialog);
	void setDisableAnimations(			bool		disableAnimations);
	void setGenerateMarkdown(			bool		generateMarkdown);

	void onCurrentThemeNameChanged(QString newThemeName);


signals:
	void jaspThemeChanged(				JaspTheme * newTheme);
	void fixedDecimalsChanged(			bool		fixedDecimals);
	void fixedDecimalsChangedString(	QString		fixedDecimals);
	void numDecimalsChanged(			int			numDecimals);
	void exactPValuesChanged(			bool		exactPValues);
	void dataAutoSynchronizationChanged(bool		dataAutoSynchronization);
	void useDefaultEditorChanged(		bool		useDefaultEditor);
	void customEditorChanged(			QString		customEditor);
	void useDefaultPPIChanged(			bool		useDefaultPPI);
	void whiteBackgroundChanged();
	void uiScaleChanged(				double		uiScale);
	void customPPIChanged(				int			customPPI);
	void defaultPPIChanged(				int			defaultPPI);
	void missingValuesChanged();
	void developerModeChanged(			bool		developerMode);
	void developerFolderChanged(		QString		developerFolder);
	void plotPPIChanged(				int			ppiForPlot,			bool	wasUserAction);
	void plotBackgroundChanged(			QString		plotBackground);
	void customThresholdScaleChanged(	bool		customThresholdScale);
	void thresholdScaleChanged(			int			thresholdScale);
	void devModRegenDESCChanged(		bool		devModRegenDESC);
	void logToFileChanged(				bool		logToFile);
	void logFilesMaxChanged(			int			logFilesMax);
	void maxFlickVelocityChanged(		int			maxFlickVelocity);
	void modulesRememberChanged(		bool		modulesRemember);
	void modulesRememberedChanged();
	void safeGraphicsChanged(			bool		safeGraphics);
	void cranRepoURLChanged(			QString		cranRepoURL);
	void useDefaultInterfaceFontChanged(bool		useDefaultInterfaceFont);
	void interfaceFontChanged(			QString		interfaceFont);
	void realInterfaceFontChanged();
	void useDefaultConsoleFontChanged(	bool		useDefaultConsoleFont);
	void consoleFontChanged(			QString		consoleFont);
	void realConsoleFontChanged();
	void useDefaultResultFontChanged(	bool		useDefaultResultFont);
	void resultFontChanged(				QString		resultFont);
	void realResultFontChanged();
	void currentThemeNameChanged(		QString		currentThemeName);
	void plotPPIPropChanged();
	void languageCodeChanged();
	void useNativeFileDialogChanged(	bool		useNativeFileDialog);
	void disableAnimationsChanged(		bool		disableAnimations);
	void generateMarkdownChanged(		bool		generateMarkdown);
	void allFontsChanged(				QStringList	allFonts);

	void animationsOnChanged();


private:
	static PreferencesModel * _singleton;

	int				_defaultPPI		= 192;
	double			_uiScale		= -1;
	QStringList		_allFonts;

	void			_loadDatabaseFont();
};

#endif // PREFERENCESDIALOG_H
