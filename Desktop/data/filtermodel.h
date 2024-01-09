#ifndef FILTERMODEL_H
#define FILTERMODEL_H

#include <QObject>
#include "utilities/qutils.h"
#include "labelfiltergenerator.h"

///
/// Backend for the filter gui
class FilterModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY( QString generatedFilter		READ generatedFilter	WRITE setGeneratedFilter	NOTIFY generatedFilterChanged	)
	Q_PROPERTY( QString rFilter				READ rFilter			WRITE setRFilter			NOTIFY rFilterChanged			)
	Q_PROPERTY( QString constructorJson		READ constructorJson	WRITE setConstructorJson	NOTIFY constructorJsonChanged	)
	Q_PROPERTY( QString constructorR		READ constructorR		WRITE setConstructorR		NOTIFY constructorRChanged		)
	Q_PROPERTY( QString statusBarText		READ statusBarText									NOTIFY statusBarTextChanged		)
	Q_PROPERTY( QString filterErrorMsg		READ filterErrorMsg									NOTIFY filterErrorMsgChanged	)
	Q_PROPERTY( bool 	hasFilter			READ hasFilter										NOTIFY hasFilterChanged			)
	Q_PROPERTY( QString defaultRFilter		READ defaultRFilter									NOTIFY defaultRFilterChanged	)

public:
	explicit					FilterModel(labelFilterGenerator * labelfilterGenerator);

				void			init();

				QString			rFilter()				const;
				QString			constructorR()			const;
				QString			statusBarText()			const	{ return _statusBarText;			}
				QString			filterErrorMsg()		const;
				QString			generatedFilter()		const;
				QString			constructorJson()		const;
	static		const char *	defaultRFilter();

				bool			hasFilter()				const	{ return rFilter() != defaultRFilter() || constructorJson() != DEFAULT_FILTER_JSON; }


	Q_INVOKABLE void			resetRFilter()				{ applyRFilter(defaultRFilter()); }
				void			sendGeneratedAndRFilter();

				void			updateStatusBar();
				void			reset();
				void			modelInit();

public slots:
	GENERIC_SET_FUNCTION(StatusBarText,			_statusBarText,			statusBarTextChanged,		QString)
	
	void setRFilter(		QString newRFilter);
	void setConstructorR(	QString newConstructorR);
	void setGeneratedFilter(QString newGeneratedFilter);
	void setConstructorJson(QString newconstructorJson);
	void setFilterErrorMsg(	QString newFilterErrorMsg);

	void applyConstructorJson(	QString constructorJson);
	void applyRFilter(			QString rFilter);

	void processFilterResult(int requestId);
	void processFilterErrorMsg(QString filterErrorMsg, int requestId);
	void rescanRFilterForColumns();

	void computeColumnSucceeded(QString columnName, QString warning, bool dataChanged);

	void dataSetPackageResetDone();
	void datasetChanged(	QStringList				changedColumns,
						QStringList				missingColumns,
						QMap<QString, QString>	changeNameColumns,
						bool					rowCountChanged,
						bool					hasNewColumns);

signals:
	void rFilterChanged();
	void hasFilterChanged();
	void constructorRChanged();
	void statusBarTextChanged();
	void filterErrorMsgChanged();
	void generatedFilterChanged();
	void constructorJsonChanged();

	void updateColumnsUsedInConstructedFilter(std::set<std::string> columnNames);

	void refreshAllAnalyses();
	void filterUpdated();

	int sendFilter(QString generatedFilter, QString rFilter);

	void defaultRFilterChanged(); //Will never be called

private:
	bool _setGeneratedFilter(const QString& newGeneratedFilter);
	bool _setRFilter(const QString& newRFilter);

private:
	labelFilterGenerator	*	_labelFilterGenerator	= nullptr;
	QString						_statusBarText			= "";

	std::set<std::string>		_columnsUsedInConstructedFilter,
								_columnsUsedInRFilter;

	int							_lastSentRequestId		= 0;

	UndoStack*					_undoStack				= nullptr;
};

#endif // FILTERMODEL_H
