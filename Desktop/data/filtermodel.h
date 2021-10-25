#ifndef FILTERMODEL_H
#define FILTERMODEL_H

#include <QObject>
#include "utilities/qutils.h"
#include "datasetpackage.h"
#include "labelfiltergenerator.h"

///
/// Backend for the filter gui
class FilterModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY( QString generatedFilter		READ generatedFilter	WRITE setGeneratedFilter	NOTIFY generatedFilterChanged	)
	Q_PROPERTY( QString rFilter				READ rFilter			WRITE setRFilter			NOTIFY rFilterChanged			)
	Q_PROPERTY( QString constructedJSON		READ constructedJSON	WRITE setConstructedJSON	NOTIFY constructedJSONChanged	)
	Q_PROPERTY( QString constructedR		READ constructedR		WRITE setConstructedR		NOTIFY constructedRChanged		)
	Q_PROPERTY( QString statusBarText		READ statusBarText									NOTIFY statusBarTextChanged		)
	Q_PROPERTY( QString filterErrorMsg		READ filterErrorMsg									NOTIFY filterErrorMsgChanged	)
	Q_PROPERTY( QString hasFilter			READ hasFilter										NOTIFY hasFilterChanged			)
	Q_PROPERTY( QString defaultRFilter		READ defaultRFilter									NOTIFY defaultRFilterChanged	)

public:
	explicit FilterModel(labelFilterGenerator * labelfilterGenerator);

	void init();

	QString rFilter()				const	{ return _rFilter;					}
	QString constructedR()			const	{ return _constructedR;				}
	QString statusBarText()			const	{ return _statusBarText;			}
	QString filterErrorMsg()		const	{ return _filterErrorMsg;			}
	QString generatedFilter()		const	{ return _generatedFilter;			}
	QString constructedJSON()		const	{ return _constructedJSON;			}
	QString defaultRFilter()		const	{ return DEFAULT_FILTER;			}

	bool	hasFilter()				const	{ return _rFilter != DEFAULT_FILTER || _constructedJSON != DEFAULT_FILTER_JSON; }


	Q_INVOKABLE void resetRFilter()				{ setRFilter(DEFAULT_FILTER); }
	void		sendGeneratedAndRFilter();

	void updateStatusBar();
	void reset();
	void modelInit();

public slots:
	GENERIC_SET_FUNCTION(StatusBarText,			_statusBarText,			statusBarTextChanged,		QString)
	GENERIC_SET_FUNCTION(FilterErrorMsg,		_filterErrorMsg,		filterErrorMsgChanged,		QString)

	void setRFilter(QString newRFilter);
	void setConstructedR(QString newConstructedR);
	void setGeneratedFilter(QString newGeneratedFilter);
	void setConstructedJSON(QString newConstructedJSON);

	void processFilterResult(std::vector<bool> filterResult, int requestId);
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
	void constructedRChanged();
	void statusBarTextChanged();
	void filterErrorMsgChanged();
	void generatedFilterChanged();
	void constructedJSONChanged();

	void updateColumnsUsedInConstructedFilter(std::set<std::string> columnNames);
	void updateGeneratedFilterWithR(QString constructedR);

	void refreshAllAnalyses();
	void filterUpdated();

	int sendFilter(QString generatedFilter, QString rFilter);

	void defaultRFilterChanged(); //Will never be called

private:
	bool _setGeneratedFilter(const QString& newGeneratedFilter);
	bool _setRFilter(const QString& newRFilter);

private:
	labelFilterGenerator	*	_labelFilterGenerator	= nullptr;
	QString						_generatedFilter		= DEFAULT_FILTER_GEN,
								_rFilter				= DEFAULT_FILTER,
								_constructedJSON		= DEFAULT_FILTER_JSON,
								_constructedR			= "",
								_statusBarText			= "",
								_filterErrorMsg			= "";

	std::set<std::string>		_columnsUsedInConstructedFilter,
								_columnsUsedInRFilter;

	int							_lastSentRequestId		= 0;
};

#endif // FILTERMODEL_H
