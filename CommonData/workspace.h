#ifndef WORKSPACE_H
#define WORKSPACE_H

#include <set>
#include "dataset.h"
#include "datasetbasenode.h"
#include "databaseinterface.h"

class Workspace : public DataSetBaseNode
{
	Q_OBJECT
	
	Q_PROPERTY(bool				dataMode			READ dataMode			WRITE setDataMode			NOTIFY dataModeChanged		)
	Q_PROPERTY(bool				showRSyntax			READ showRSyntax		WRITE setShowRSyntax		NOTIFY showRSyntaxChanged	)
	Q_PROPERTY(DataSet		*	shownDataSet		READ shownDataSet									NOTIFY shownDataSetChanged	)
	Q_PROPERTY(Column		*	shownColumn			READ shownColumn		WRITE setShownColumn		NOTIFY shownColumnChanged	)
	Q_PROPERTY(Filter		*	shownFilter			READ shownFilter		WRITE setShownFilter		NOTIFY shownFilterChanged	)
	Q_PROPERTY(VariableInfo *	varInfo				READ varInfo										CONSTANT					)
	Q_PROPERTY(QVariantList	inputFilterDropDownList READ inputFilterDropDownList						NOTIFY inputFilterDropDownListChanged	)
	///Every filter of every dataset in the workspace as {value: filterId, label: "DataSet - Filter"} entries,
	///for the multi-dataset aware dataset/filter selection (see VariablesForm::dataSetSelection). Selecting a
	///dataset always means selecting one of its filters, hence filters and not just datasets here.
	Q_PROPERTY(QVariantList	dataSetFilterDropDownList READ dataSetFilterDropDownList					NOTIFY dataSetFilterDropDownListChanged	)
	
	// Emit signals also in refresh
	
public:
	explicit Workspace(QObject *parent = nullptr);
	~Workspace();
	
			DatabaseInterface	 &	db();
	const	DatabaseInterface	 &	db() const;
	
			bool					dataMode()				const	{ return _dataMode;		}
			bool					showRSyntax()			const	{ return _showRSyntax;	}
			
			int						rowCount(		const QModelIndex &parent = QModelIndex())										const	override { return _dataSets.size(); }
			int						columnCount(	const QModelIndex &parent = QModelIndex())										const	override { return 1; }
			QVariant				data(			const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
			
			VariableInfo		*	varInfo() const { return _varInfo; }
			
			void					setDataMode(		bool mode);
			void					setShowRSyntax(		bool showRSyntax);
			
			void					dbLoad(std::function<void(float)> progressCallback = [](float){}, Version doUpgradeFrom = Version());
			void					dbUpdate();
			void					dbDelete();
			
			bool					checkForUpdates(std::function<void(float)> progressCallback = [](float){});
			
				
			DataSet				*	shownDataSet()	const;
			DataSets				dataSets()		const;
			DataSet				*	dataSetById(int id) const;
			DataSet				*	dataSetByName(const std::string & name) const;
			///< Exact match on the user facing title (unique per Workspace::setTitle); syntax-mode
			///< dataset selection refers to datasets by name.
			DataSet				*	dataSetByTitle(const QString & title) const;
			Filter				*	filterById(int id) const;
			///Returns title if no other dataset already has that title, otherwise appends " (n)" with an incrementing n until it is unique. exclude lets a dataset check against the others without matching against its own current title.
			QString					makeDataSetTitleUnique(const QString & title, DataSet * exclude = nullptr) const;
			
			Column				*	shownColumn() const;
			Filter				*	shownFilter() const;
			void					setShownColumn(Column *newShownColumn);
			void					setShownFilter(Filter * newShownFilter);
			void					initializeComputedColumns();
	///True if making 'me' depend on 'target' (as defaultInputFilterId) would create a cycle among
	///the computed datasets. Used by DataSet::setDefaultInputFilterId to refuse loops.
	bool							wouldCreateComputedDataSetLoop(DataSet * me, DataSet * target) const;
	///True if any cycle exists among computed datasets; fills errorMessage. Used as an anti-livelock
	///sweep before running the recompute cascade.
	bool							computedDataSetsHaveLoop(std::string & errorMessage) const;
	static	Workspace			*	singleton() { return _singleton; }
	
	
public slots:
			void					refresh();
			DataSet				*	createDataSet();
			Column				*	createComputedColumn(const std::string & name, int dataSetId, int analysisId = -1, columnType type = columnType::unknown, computedColumnType desiredType = computedColumnType::analysis);
			DataSet				*	createComputedDataSet(const std::string & name, int defaultInputFilterId, computedColumnType desiredType = computedColumnType::rCode);
			Q_INVOKABLE int						shownDataSetId() const	{ return shownDataSet() ? shownDataSet()->id() : -1; }
			Q_INVOKABLE int						dataSetIdByName(const QString & name) const		{ DataSet * ds = dataSetByName(fq(name)); return ds ? ds->id() : -1; }
			Q_INVOKABLE QString					dataSetNameById(int id) const				{ DataSet * ds = dataSetById(id); return ds ? ds->name() : QString(); }
			Q_INVOKABLE QStringList				dataSetNames() const;
			QVariantList						inputFilterDropDownList() const;
			QVariantList						dataSetFilterDropDownList() const;
			Q_INVOKABLE void					setDataSetComputed(const QString & name, bool computed);
			void					setShownDataSet(QString	  name);
			void					setShownDataSet(DataSet * dataSet);
			void					setShownDataSet(int		  dataSetId);
			void					deleteShownDataSet();
			void					showFilter(int id);
			void					onShownFilterChanged(DataSet * data);
			void					refreshAllCompCols(Filter * f);
			void					updateComputedColumnDependenciesForAnalysis(int analysisId, const stringset & usedVariables);
			void					computedColumnSucceeded(int dataSetId, QString columnName, QString warning, bool dataChanged);
			void					computedDataSetSucceeded(int dataSetId, QString warning, bool dataChanged);
			void					initializeComputedDatasets();
			
signals:
			void					dataSetCreated(int dataSetId);
			void					dataSetRemoved(int dataSetId);
			void					dataSetTitleChanged(int dataSetId);
			void					filterByNameDone(int dataSetId, const QString & name, const QString & error);
			void					dataModeChanged(bool dataMode);
			void					showRSyntaxChanged(bool showIt);
			void					shownDataSetChanged(DataSet * dataSet);
			void					shownFilterChanged();
			void					manualEditMade(); 
			void					datasetChanged(				int						dataSetId,
																QStringList				changedColumns,
																QStringList				missingColumns,
																QMap<QString, QString>	changeNameColumns,
																bool					rowCountChanged,
																bool					hasNewColumns); 
			void					labelsReordered(			QString columnName);
			void					labelFilterChanged();
			QString					askPassword(	QString title, QString message);
			bool					showYesNo(		QString title, QString message);
			void					allFiltersReset();
			void					showWarning(						QString title, QString msg);
			void					descriptionChanged();
			void					dataFileChanged();
			void					databaseJsonChanged();
			void					dataFileSynchChanged();
			void					dataTimestampChanged();
			void					columnsLabelFilteredCountChanged();
			void					refreshAllAnalyses(Filter * f);
			void					runComputedColumn(int dataSetid, QString columnName, QString code, enum columnType columnType);
			void					runComputedDataSet(int dataSetid, QString code, int defaultInputFilterId);
			void					sendFilter(			int dataSetID, const QString & generatedFilter, const QString & filter);
			void					sendFilterByName(	int dataSetID, const QString & name, const QString & module = "*");
			void					filtersCountChanged();
void					enableModified();
			void					shownColumnChanged();
			void					checkForDependentAnalyses(Column * column);
			void					showAnalysis(			int			analysisId);
			void					emptyValuesChanged();
			void					inputFilterDropDownListChanged();
			void					dataSetFilterDropDownListChanged();
			
	
			
private:
	std::map<int,DataSet*>			_dataSets;
	DataSet						*	_shownDataSet			= nullptr;
	VariableInfo				*	_varInfo				= nullptr;
	///Shared builder for inputFilterDropDownList()/dataSetFilterDropDownList(); the filter of the dataset
	///with excludeDataSetId (-1 for none) is left out.
	QVariantList					_filterDropDownList(int excludeDataSetId) const;
	bool							_showRSyntax			= false,
									_dataMode				= false,
									_inRefresh				= false; //instance flag (not static): works across Workspace instances
	static Workspace			*	_singleton;

};

#endif // WORKSPACE_H
