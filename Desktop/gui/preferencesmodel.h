#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include <QObject>
#include <QFont>
#include "column.h"
#include "utilities/qutils.h"

class JaspTheme;

///
/// Interface between QML and Settings, mostly templated functions to link through directly.
class PreferencesModel : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY(bool			fixedDecimals			READ fixedDecimals				WRITE setFixedDecimals				NOTIFY fixedDecimalsChanged				)
	Q_PROPERTY(int			numDecimals				READ numDecimals				WRITE setNumDecimals				NOTIFY numDecimalsChanged				)
	Q_PROPERTY(bool			exactPValues			READ exactPValues				WRITE setExactPValues				NOTIFY exactPValuesChanged				)
	Q_PROPERTY(bool			normalizedNotation		READ normalizedNotation			WRITE setNormalizedNotation			NOTIFY normalizedNotationChanged		)
	Q_PROPERTY(bool			dataAutoSynchronization	READ dataAutoSynchronization	WRITE setDataAutoSynchronization	NOTIFY dataAutoSynchronizationChanged	)
	Q_PROPERTY(bool			useDefaultEditor		READ useDefaultEditor			WRITE setUseDefaultEditor			NOTIFY useDefaultEditorChanged			)
	Q_PROPERTY(QString		customEditor			READ customEditor				WRITE setCustomEditor				NOTIFY customEditorChanged				)
	Q_PROPERTY(bool			useDefaultPPI			READ useDefaultPPI				WRITE setUseDefaultPPI				NOTIFY useDefaultPPIChanged				)
	Q_PROPERTY(int			customPPI				READ customPPI					WRITE setCustomPPI					NOTIFY customPPIChanged					)
	Q_PROPERTY(bool			whiteBackground			READ whiteBackground			WRITE setWhiteBackground			NOTIFY whiteBackgroundChanged			)
	Q_PROPERTY(QString		plotBackground			READ plotBackground				WRITE setPlotBackground				NOTIFY plotBackgroundChanged			)
	Q_PROPERTY(double		uiScale					READ uiScale					WRITE setUiScale					NOTIFY uiScaleChanged					)
	Q_PROPERTY(int			defaultPPI				READ defaultPPI					WRITE setDefaultPPI					NOTIFY defaultPPIChanged				)
	Q_PROPERTY(bool			developerMode			READ developerMode				WRITE setDeveloperMode				NOTIFY developerModeChanged				)
	Q_PROPERTY(QString		developerFolder			READ developerFolder			WRITE setDeveloperFolder			NOTIFY developerFolderChanged			)
	Q_PROPERTY(bool			customThresholdScale	READ customThresholdScale		WRITE setCustomThresholdScale		NOTIFY customThresholdScaleChanged		)
	Q_PROPERTY(int			thresholdScale			READ thresholdScale				WRITE setThresholdScale				NOTIFY thresholdScaleChanged			)
	Q_PROPERTY(bool			logToFile				READ logToFile					WRITE setLogToFile					NOTIFY logToFileChanged					)
	Q_PROPERTY(int			logFilesMax				READ logFilesMax				WRITE setLogFilesMax				NOTIFY logFilesMaxChanged				)
	Q_PROPERTY(int			maxFlickVelocity		READ maxFlickVelocity			WRITE setMaxFlickVelocity			NOTIFY maxFlickVelocityChanged			)
	Q_PROPERTY(bool			modulesRemember			READ modulesRemember			WRITE setModulesRemember			NOTIFY modulesRememberChanged			)
	Q_PROPERTY(QStringList	modulesRemembered		READ modulesRemembered			WRITE setModulesRemembered			NOTIFY modulesRememberedChanged			)
	Q_PROPERTY(bool			safeGraphics			READ safeGraphics				WRITE setSafeGraphics				NOTIFY safeGraphicsChanged				)
	Q_PROPERTY(QString		cranRepoURL				READ cranRepoURL				WRITE setCranRepoURL				NOTIFY cranRepoURLChanged				)
	Q_PROPERTY(bool			githubPatUseDefault		READ githubPatUseDefault		WRITE setGithubPatUseDefault		NOTIFY githubPatUseDefaultChanged		)
	Q_PROPERTY(QString		githubPatCustom			READ githubPatCustom			WRITE setGithubPatCustom			NOTIFY githubPatCustomChanged			)
	Q_PROPERTY(QString		interfaceFont			READ interfaceFont				WRITE setInterfaceFont				NOTIFY interfaceFontChanged				)
	Q_PROPERTY(QString		codeFont				READ codeFont					WRITE setCodeFont					NOTIFY codeFontChanged					)
	Q_PROPERTY(QString		resultFont				READ resultFont					WRITE setResultFont					NOTIFY resultFontChanged				)
	Q_PROPERTY(QString		currentThemeName		READ currentThemeName			WRITE setCurrentThemeName			NOTIFY currentThemeNameChanged			)
	Q_PROPERTY(bool			useNativeFileDialog		READ useNativeFileDialog		WRITE setUseNativeFileDialog		NOTIFY useNativeFileDialogChanged		)
	Q_PROPERTY(bool			disableAnimations		READ disableAnimations			WRITE setDisableAnimations			NOTIFY disableAnimationsChanged			)
	Q_PROPERTY(bool			generateMarkdown		READ generateMarkdown			WRITE setGenerateMarkdown			NOTIFY generateMarkdownChanged			)
	Q_PROPERTY(QStringList	missingValues			READ missingValues													NOTIFY missingValuesChanged				)
	Q_PROPERTY(int			plotPPI					READ plotPPI														NOTIFY plotPPIPropChanged				)
	Q_PROPERTY(bool			animationsOn			READ animationsOn													NOTIFY animationsOnChanged				)
	Q_PROPERTY(QString		languageCode			READ languageCode													NOTIFY languageCodeChanged				)
	Q_PROPERTY(QStringList	allCodeFonts			READ allCodeFonts				CONSTANT																	)
	Q_PROPERTY(QString		defaultInterfaceFont	READ defaultInterfaceFont		CONSTANT																	)
	Q_PROPERTY(QString		defaultCodeFont			READ defaultCodeFont			CONSTANT																	)
	Q_PROPERTY(QString		defaultResultFont		READ defaultResultFont			CONSTANT																	)
	Q_PROPERTY(QStringList	allInterfaceFonts		READ allInterfaceFonts			CONSTANT																	)
	Q_PROPERTY(QStringList	allResultFonts			READ allResultFonts				CONSTANT																	)
	Q_PROPERTY(int			maxEngines				READ maxEngines					WRITE setMaxEngines					NOTIFY maxEnginesChanged				)
	Q_PROPERTY(int			maxEnginesAdmin			READ maxEnginesAdmin												NOTIFY maxEnginesAdminChanged			)
	Q_PROPERTY(bool			windowsNoBomNative		READ windowsNoBomNative			WRITE setWindowsNoBomNative			NOTIFY windowsNoBomNativeChanged		)
	Q_PROPERTY(bool			dbShowWarning			READ dbShowWarning				WRITE setDbShowWarning				NOTIFY dbShowWarningChanged				)
	Q_PROPERTY(QString		dataLabelNA				READ dataLabelNA				WRITE setDataLabelNA				NOTIFY dataLabelNAChanged				)
	Q_PROPERTY(bool			guiQtTextRender			READ guiQtTextRender			WRITE setGuiQtTextRender			NOTIFY guiQtTextRenderChanged			)
	Q_PROPERTY(bool			reportingMode			READ reportingMode				WRITE setReportingMode				NOTIFY reportingModeChanged				)

public:
	explicit	 PreferencesModel(QObject *parent = 0);

	int			customPPI()								const;
	int			numDecimals()							const;
	int			defaultPPI()							const	{ return _defaultPPI; }
	int			plotPPI()								const	{ return useDefaultPPI() ? defaultPPI() : customPPI();	}
	bool		fixedDecimals()							const;
	bool		exactPValues()							const;
	bool		normalizedNotation()					const;
	bool		dataAutoSynchronization()				const;
	bool		useDefaultEditor()						const;
	bool		useDefaultPPI()							const;
	bool		whiteBackground()						const;
	QString		plotBackground()						const;
	double		uiScale()								;
	QString		customEditor()							const;
	QString		developerFolder()						const;
	QString		fixedDecimalsForJS()					const;
	QStringList	missingValues()							const;
	bool		customThresholdScale()					const;
	int			thresholdScale()						const;
	bool		logToFile()								const;
	int			logFilesMax()							const;
	int			maxFlickVelocity()						const;
	bool		modulesRemember()						const;
	QStringList	modulesRemembered()						const;
	bool		safeGraphics()							const;
	QString		cranRepoURL()							const;
	QString		githubPatResolved()						const;
	QString		githubPatCustom()						const;
	bool		githubPatUseDefault()					const;
	QString		interfaceFont()							const;
	QString		codeFont()								const;
	QString		resultFont(bool forWebEngine = false)	const;
	QString		currentThemeName()						const;
	QString		languageCode()							const;
	bool		useNativeFileDialog()					const;
	bool		disableAnimations()						const;
	bool		animationsOn()							const { return !disableAnimations() && !safeGraphics(); }
	bool		generateMarkdown()						const;
	QStringList allInterfaceFonts()						const { return _allInterfaceFonts; }
	QStringList allCodeFonts()							const { return _allCodeFonts; }
	QStringList allResultFonts()						const { return _allResultFonts; }
	QString		defaultResultFont()						const;
	QString		defaultInterfaceFont()					const;
	QString		defaultCodeFont()						const;
	int			maxEngines()							const;
	bool		windowsNoBomNative()					const;
	bool		dbShowWarning()							const;
	QString		dataLabelNA()							const;
	bool		guiQtTextRender()						const;
	bool		reportingMode()							const;
	void		zoomIn();
	void		zoomOut();
	void		zoomReset();
	int 		maxEnginesAdmin() 						const;

public slots:
	bool developerMode()								const; //Some 
	void setUiScale(					double		uiScale);
	void setCustomPPI(					int			customPPI);
	void setDefaultPPI(					int			defaultPPI);
	void setNumDecimals(				int			numDecimals);
	void setExactPValues(				bool		exactPValues);
	void setNormalizedNotation(			bool		normalizedNotation);
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
	void setLogToFile(					bool		logToFile);
	void setLogFilesMax(				int			logFilesMax);
	void setMaxFlickVelocity(			int			maxFlickVelocity);
	void setModulesRemember(			bool		modulesRemember);
	void setModulesRemembered(			QStringList modulesRemembered);
	void setSafeGraphics(				bool		safeGraphics);
	void setCranRepoURL(				QString		cranRepoURL);
	void setGithubPatUseDefault(		bool		useDefault);
	void setGithubPatCustom(			QString		pat);
	void moduleEnabledChanged(			QString		moduleName, bool enabled);
	void onUseDefaultPPIChanged(		bool		useDefault);
	void onCustomPPIChanged(			int);
	void onDefaultPPIChanged(			int);
	void setCurrentThemeName(			QString		currentThemeName);
	void setInterfaceFont(				QString		interfaceFont);
	void setCodeFont(					QString		codeFont);
	void setResultFont(					QString		resultFont);
	void setUseNativeFileDialog(		bool		useNativeFileDialog);
	void setDisableAnimations(			bool		disableAnimations);
	void setGenerateMarkdown(			bool		generateMarkdown);
	void resetRememberedModules(		bool		clear);
	void setMaxEngines(					int			maxEngines);
	void setWindowsNoBomNative(			bool		windowsNoBomNative);
	void setDbShowWarning(				bool		dbShowWarning);
	void setDataLabelNA(				QString		dataLabelNA);
	void setGuiQtTextRender(			bool		newGuiQtTextRender);
	void onGuiQtTextRenderChanged(		bool		newGuiQtTextRenderSetting);
	void setReportingMode(				bool		reportingMode);
	void currentThemeNameHandler();
	
signals:
	void fixedDecimalsChanged(			bool		fixedDecimals);
	void fixedDecimalsChangedString(	QString		fixedDecimals);
	void numDecimalsChanged(			int			numDecimals);
	void exactPValuesChanged(			bool		exactPValues);
	void normalizedNotationChanged(		bool		normalizedNotation);
	void dataAutoSynchronizationChanged(bool		dataAutoSynchronization);
	void useDefaultEditorChanged(		bool		useDefaultEditor);
	void customEditorChanged(			QString		customEditor);
	void useDefaultPPIChanged(			bool		useDefaultPPI);
	void whiteBackgroundChanged();
	void customPPIChanged(				int			customPPI);
	void defaultPPIChanged(				int			defaultPPI);
	void missingValuesChanged();
	void developerModeChanged(			bool		developerMode);
	void developerFolderChanged(		QString		developerFolder);
	void plotPPIChanged(				int			ppiForPlot,			bool	wasUserAction);
	void plotBackgroundChanged(			QString		plotBackground);
	void customThresholdScaleChanged(	bool		customThresholdScale);
	void thresholdScaleChanged(			int			thresholdScale);
	void logToFileChanged(				bool		logToFile);
	void logFilesMaxChanged(			int			logFilesMax);
	void modulesRememberChanged(		bool		modulesRemember);
	void modulesRememberedChanged();
	void safeGraphicsChanged(			bool		safeGraphics);
	void cranRepoURLChanged(			QString		cranRepoURL);
	void githubPatUseDefaultChanged(	bool		githubPatUseDefault);
	void githubPatCustomChanged();
	void codeFontChanged(				QString		codeFont);
	void resultFontChanged(				QString		resultFont);
	void currentThemeNameChanged(		QString		currentThemeName);
	void plotPPIPropChanged();
	void languageCodeChanged();
	void useNativeFileDialogChanged(	bool		useNativeFileDialog);
	void disableAnimationsChanged(		bool		disableAnimations);
	void generateMarkdownChanged(		bool		generateMarkdown);
	void animationsOnChanged();
	void lcCtypeChanged();
	void restartAllEngines();
	void maxEnginesChanged(				int			maxEngines);
	void windowsNoBomNativeChanged(		bool		windowsNoBomNative);
	void dbShowWarningChanged(			bool		dbShowWarning);
	void maxEnginesAdminChanged();
	void dataLabelNAChanged(			QString		dataLabelNA);
	void guiQtTextRenderChanged(		bool		guiQtTextRender);
	void reportingModeChanged(			bool		reportingMode);
	void uiScaleChanged(				float		uiScale);
	void maxFlickVelocityChanged(		float		flickVelo);
	void currentJaspThemeChanged();
	void currentThemeReady();
	void interfaceFontChanged();

private slots:
	void dataLabelNAChangedSlot(QString label);
	
private:
	int				_defaultPPI		= 192;
	double			_uiScale		= -1;
	QStringList		_allFonts,
					_allInterfaceFonts,
					_allResultFonts,
					_allCodeFonts;
	bool			_githubPatCustom; //Should be initialized on prefs construction

	void			_loadDatabaseFont();
	QString			_checkFontList(QString fonts) const;
	
	
};

#endif // PREFERENCESDIALOG_H
