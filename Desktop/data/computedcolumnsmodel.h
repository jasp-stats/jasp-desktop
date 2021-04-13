#ifndef COMPUTEDCOLUMNSCODEITEM_H
#define COMPUTEDCOLUMNSCODEITEM_H

#include <QQuickItem>
#include <QObject>
#include "computedcolumns.h"
#include "datasetpackage.h"
#include "analysis/analyses.h"

class ComputedColumnsModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		computeColumnUsesRCode		READ computeColumnUsesRCode													NOTIFY computeColumnUsesRCodeChanged	)
	Q_PROPERTY(QString	computeColumnRCode			READ computeColumnRCode				WRITE setComputeColumnRCode				NOTIFY computeColumnRCodeChanged		)
	Q_PROPERTY(QString	computeColumnJson			READ computeColumnJson														NOTIFY computeColumnJsonChanged			)
	Q_PROPERTY(QString	computeColumnError			READ computeColumnError														NOTIFY computeColumnErrorChanged		)
	Q_PROPERTY(QString	computeColumnNameSelected	READ computeColumnNameSelected		WRITE setComputeColumnNameSelected		NOTIFY computeColumnNameSelectedChanged )
	Q_PROPERTY(bool		datasetLoaded				READ datasetLoaded															NOTIFY datasetLoadedChanged				)
	Q_PROPERTY(QString	lastCreatedColumn			READ lastCreatedColumn														NOTIFY lastCreatedColumnChanged			)
	Q_PROPERTY(QString	showThisColumn				READ showThisColumn					WRITE setShowThisColumn					NOTIFY showThisColumnChanged			)

public:
	explicit	ComputedColumnsModel();

				bool	datasetLoaded()					{ return DataSetPackage::pkg()->hasDataSet(); }
				QString	computeColumnRCode();
				QString computeColumnRCodeCommentStripped();
				QString computeColumnError();
				QString computeColumnNameSelected();
				QString computeColumnJson();
				QString lastCreatedColumn() const { return _lastCreatedColumn;	}
				QString showThisColumn()	const { return _showThisColumn;		}
				bool	computeColumnUsesRCode();

				void	setComputeColumnRCode(QString newCode);
				void	setComputeColumnNameSelected(QString newName);
				void	setComputeColumnJson(QString newJson);

	Q_INVOKABLE	void	removeColumn();
	Q_INVOKABLE void	sendCode(QString code);
	Q_INVOKABLE void	sendCode(QString code, QString json);

	Q_INVOKABLE bool	isColumnNameFree(QString name)						{ return DataSetPackage::pkg()->isColumnNameFree(name.toStdString()); }

				ComputedColumn*		createComputedColumn(const std::string & name, int columnType, ComputedColumn::computedType computeType, Analysis * analysis = nullptr);
	Q_INVOKABLE void				createComputedColumn(const QString     & name, int columnType, bool jsonPlease)	{ createComputedColumn(fq(name), columnType, jsonPlease ? ComputedColumn::computedType::constructorCode : ComputedColumn::computedType::rCode);	}

				bool				areLoopDependenciesOk(std::string columnName);
				bool				areLoopDependenciesOk(std::string columnName, std::string code);

	Q_INVOKABLE bool				showAnalysisFormForColumn(QString columnName);

				static ComputedColumnsModel * singleton()		{ return _singleton; }
				static ComputedColumns		* computedColumns()	{ return ComputedColumns::singleton(); }

private:
				void				validate(QString name);
				void				emitHeaderDataChanged(QString name);
				void				revertToDefaultInvalidatedColumns();
				void				checkForDependentAnalyses(std::string columnName);
				void				invalidate(QString name);
				void				invalidateDependents(std::string columnName);
				void				checkForDependentColumnsToBeSent(std::string columnName, bool refreshMe = false);
				void				emitSendComputeCode(QString columnName, QString code, columnType colType);

signals:
				void	datasetLoadedChanged();
				void	computeColumnRCodeChanged();
				void	computeColumnErrorChanged();
				void	computeColumnJsonChanged();
				void	refreshColumn(QString columnName);
				void	computeColumnNameSelectedChanged();
				void	headerDataChanged(Qt::Orientation orientation, int first, int last);
				void	sendComputeCode(QString columnName, QString code, columnType columnType);
				void	computeColumnUsesRCodeChanged();
				void	refreshData();
				void	showAnalysisForm(Analysis *analysis);
				void	lastCreatedColumnChanged(QString lastCreatedColumn);
				void	dataColumnAdded(QString columnName);
				void	showThisColumnChanged(QString showThisColumn);

public slots:
				void				computeColumnSucceeded(QString columnName, QString warning, bool dataChanged);
				void				computeColumnFailed(QString columnName, QString error);
				void				checkForDependentColumnsToBeSentSlot(std::string columnName)					{ checkForDependentColumnsToBeSent(columnName, false); }
				ComputedColumn *	requestComputedColumnCreation(const std::string& columnName, Analysis * analysis);
				void				requestColumnCreation(const std::string& columnName, Analysis * analysis, columnType type);
				void				requestComputedColumnDestruction(const std::string& columnName);
				void				recomputeColumn(QString columnName);
				void				setLastCreatedColumn(QString lastCreatedColumn);
				void				analysisRemoved(Analysis * analysis);
				void				setShowThisColumn(QString showThisColumn);
				void				datasetChanged(	QStringList				changedColumns,
													QStringList				missingColumns,
													QMap<QString, QString>	changeNameColumns,
													bool					rowCountChanged,
													bool					hasNewColumns);

private:
	static ComputedColumnsModel * _singleton;

	QString					_currentlySelectedName	= "",
							_lastCreatedColumn		= "",
							_showThisColumn			= "";
};

#endif // COMPUTEDCOLUMNSCODEITEM_H

