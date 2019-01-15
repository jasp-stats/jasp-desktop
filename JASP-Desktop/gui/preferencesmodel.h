#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include <QObject>
#include "column.h"
#include "utilities/qutils.h"

class PreferencesModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY(bool		fixedDecimals			READ fixedDecimals				WRITE setFixedDecimals				NOTIFY fixedDecimalsChanged				)
	Q_PROPERTY(int		numDecimals				READ numDecimals				WRITE setNumDecimals				NOTIFY numDecimalsChanged				)
	Q_PROPERTY(bool		exactPValues			READ exactPValues				WRITE setExactPValues				NOTIFY exactPValuesChanged				)
	Q_PROPERTY(bool		dataAutoSynchronization	READ dataAutoSynchronization	WRITE setDataAutoSynchronization	NOTIFY dataAutoSynchronizationChanged	)
	Q_PROPERTY(bool		useDefaultEditor		READ useDefaultEditor			WRITE setUseDefaultEditor			NOTIFY useDefaultEditorChanged			)
	Q_PROPERTY(QString	customEditor			READ customEditor				WRITE setCustomEditor				NOTIFY customEditorChanged				)
	Q_PROPERTY(bool		useDefaultPPI			READ useDefaultPPI				WRITE setUseDefaultPPI				NOTIFY useDefaultPPIChanged				)
	Q_PROPERTY(int		customPPI				READ customPPI					WRITE setCustomPPI					NOTIFY customPPIChanged)
	Q_PROPERTY(bool		whiteBackground			READ whiteBackground			WRITE setWhiteBackground			NOTIFY whiteBackgroundChanged			)
	Q_PROPERTY(double	uiScale					READ uiScale					WRITE setUiScale					NOTIFY uiScaleChanged					)


public:
	explicit	 PreferencesModel(QObject *parent = 0);
				~PreferencesModel();

	bool	fixedDecimals()				const;
	int		numDecimals()				const;
	bool	exactPValues()				const;
	bool	dataAutoSynchronization()	const;
	bool	useDefaultEditor()			const;
	QString	customEditor()				const;
	bool	useDefaultPPI()				const;
	bool	whiteBackground()			const;
	double	uiScale()					const;
	int		customPPI()					const;
private:
	std::vector<std::string> getStdVectorFromEmptyValueList();
	QString getTokenStringFromEmptyValueList();
	bool addStringToEmptyValueList(const QString &in);
	void checkEmptyValueList();
	void fillMissingValueList(const std::vector<std::string> &emptyValues);

public slots:
	void savePreferences();
	void browseSpreadsheetEditor();

	void setFixedDecimals(bool fixedDecimals);
	void setNumDecimals(int numDecimals);
	void setExactPValues(bool exactPValues);
	void setDataAutoSynchronization(bool dataAutoSynchronization);
	void setUseDefaultEditor(bool useDefaultEditor);
	void setCustomEditor(QString customEditor);
	void setUseDefaultPPI(bool useDefaultPPI);
	void setWhiteBackground(bool whiteBackground);
	void setUiScale(double uiScale);
	void setCustomPPI(int customPPI);

signals:
	void fixedDecimalsChanged(bool fixedDecimals);
	void numDecimalsChanged(int numDecimals);
	void exactPValuesChanged(bool exactPValues);
	void dataAutoSynchronizationChanged(bool dataAutoSynchronization);
	void useDefaultEditorChanged(bool useDefaultEditor);
	void customEditorChanged(QString customEditor);
	void useDefaultPPIChanged(bool useDefaultPPI);
	void whiteBackgroundChanged(bool whiteBackground);
	void uiScaleChanged(double uiScale);
	void customPPIChanged(int customPPI);


};

#endif // PREFERENCESDIALOG_H
