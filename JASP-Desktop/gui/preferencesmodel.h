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
	Q_PROPERTY(QFont		defaultFont				READ defaultFont				WRITE setDefaultFont				NOTIFY defaultFontChanged				)
	Q_PROPERTY(QString		currentThemeName		READ currentThemeName			WRITE setCurrentThemeName			NOTIFY currentThemeNameChanged			)

public:
	explicit	 PreferencesModel(QObject *parent = 0);
				~PreferencesModel();

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
	bool		developerMode()				const;
	double		uiScale()					const;
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
	QFont		defaultFont()				const	{ return _defaultFont;	}
	QString		currentThemeName()			const;

	void		missingValuesToStdVector(std::vector<std::string> & out) const;
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

	void setDefaultFont(QFont defaultFont);
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
	void whiteBackgroundChanged(		bool		whiteBackground);
	void uiScaleChanged(				double		uiScale);
	void customPPIChanged(				int			customPPI);
	void defaultPPIChanged(				int			defaultPPI);
	void missingValuesChanged();
	void developerModeChanged(			bool		developerMode);
	void developerFolderChanged(		QString		developerFolder);
	void plotPPIChanged(				int			ppiForPlot,			bool	wasUserAction);
	void plotBackgroundChanged(			QString		background);
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
	void defaultFontChanged(			QFont		defaultFont);
	void currentThemeNameChanged(		QString		currentThemeName);
	void plotPPIPropChanged();

private:
	int		_defaultPPI		= 192;
	QFont	_defaultFont	= QFont("SansSerif");
};

#endif // PREFERENCESDIALOG_H
