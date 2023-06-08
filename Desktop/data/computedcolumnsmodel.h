#ifndef COMPUTEDCOLUMNSCODEITEM_H
#define COMPUTEDCOLUMNSCODEITEM_H

#include <QQuickItem>
#include <QObject>
#include "datasetpackage.h"
#include "analysis/analyses.h"

/// 
/// A model for use by the computed columns editor in QML
/// It can only show the relevant information for a single computed column at a time
class ComputedColumnsModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		computeColumnUsesRCode		READ computeColumnUsesRCode													NOTIFY computeColumnUsesRCodeChanged	)
	Q_PROPERTY(QString	computeColumnRCode			READ computeColumnRCode				WRITE setComputeColumnRCode				NOTIFY computeColumnRCodeChanged		)
	Q_PROPERTY(QString	computeColumnJson			READ computeColumnJson														NOTIFY computeColumnJsonChanged			)
	Q_PROPERTY(QString	computeColumnError			READ computeColumnError														NOTIFY computeColumnErrorChanged		)
	Q_PROPERTY(QString	computeColumnNameSelected	READ computeColumnNameSelected		WRITE setComputeColumnNameSelected		NOTIFY computeColumnNameSelectedChanged )
	Q_PROPERTY(bool		datasetLoaded				READ datasetLoaded															NOTIFY refreshProperties				)

public:
	explicit	ComputedColumnsModel();

				bool				datasetLoaded()					{ return DataSetPackage::pkg()->hasDataSet();	}
				DataSet			*	dataSet()						{ return DataSetPackage::pkg()->dataSet();		}
				const Columns	&	computedColumns() const			{ return DataSetPackage::pkg()->dataSet()->computedColumns(); }
				QString				computeColumnRCode();
				QString				computeColumnRCodeCommentStripped();
				QString				computeColumnError();
				QString				computeColumnNameSelected();
				QString				computeColumnJson();
				Column			*	column() const;
				bool				computeColumnUsesRCode();

				void				selectColumn(					Column		  * column);
				void				setComputeColumnRCode(			const QString & newCode);
				void				setComputeColumnNameSelected(	const QString & newName);
				void				setComputeColumnJson(			const QString & newJson);

	Q_INVOKABLE void				sendCode(const QString & code);
	Q_INVOKABLE void				sendCode(const QString & code, const QString & json);

	Q_INVOKABLE	void				removeColumn();
	Q_INVOKABLE bool				isColumnNameFree(const QString & name)						{ return DataSetPackage::pkg()->isColumnNameFree(name.toStdString()); }

				Column			*	createComputedColumn(const std::string & name, int columnType, computedColumnType computeType, Analysis * analysis = nullptr);
	Q_INVOKABLE void				createComputedColumn(const QString     & name, int columnType, bool jsonPlease)	{ createComputedColumn(fq(name), columnType, jsonPlease ? computedColumnType::constructorCode : computedColumnType::rCode);	}

				bool				areLoopDependenciesOk(const std::string & columnName);
				bool				areLoopDependenciesOk(const std::string & columnName, const std::string & code);

	Q_INVOKABLE bool				showAnalysisFormForColumn(const QString & columnName);

	static		ComputedColumnsModel * singleton()		{ return _singleton; }


private:
				void				revertToDefaultInvalidatedColumns();
				void				validate(							const QString		& name);
				void				emitHeaderDataChanged(				const QString		& name);
				void				checkForDependentAnalyses(			const std::string	& columnName);
				void				invalidate(							const QString		& name);
				void				invalidateDependents(				const std::string	& columnName);
				void				emitSendComputeCode(				const QString		& columnName, const QString & code, columnType colType);

signals:
				void	refreshProperties();
				void	computeColumnRCodeChanged();
				void	computeColumnErrorChanged();
				void	computeColumnJsonChanged();
				void	refreshColumn(QString columnName);
				void	computeColumnNameSelectedChanged();
				void	headerDataChanged(Qt::Orientation orientation, int first, int last);
				void	sendComputeCode(QString columnName, QString code, columnType columnType);
				void	computeColumnUsesRCodeChanged();
				void	showAnalysisForm(Analysis *analysis);
				void	dataColumnAdded(QString columnName);
				void	refreshData();

public slots:
				void	checkForDependentColumnsToBeSent(QString columnName, bool refreshMe = false);
				void	computeColumnSucceeded(QString columnName, QString warning, bool dataChanged);
				void	computeColumnFailed(QString columnName, QString error);
				void	checkForDependentColumnsToBeSentSlot(QString columnName)					{ checkForDependentColumnsToBeSent(columnName, false); }
				void	recomputeColumn(QString columnName);
				void	analysisRemoved(Analysis * analysis);
				void	datasetChanged(	QStringList				changedColumns,
										QStringList				missingColumns,
										QMap<QString, QString>	changeNameColumns,
										bool					rowCountChanged,
										bool					hasNewColumns);
private slots:
				void	onDataSetChanged();

private:
	static	ComputedColumnsModel	* _singleton;
			Column					* _selectedColumn = nullptr;

};

#endif // COMPUTEDCOLUMNSCODEITEM_H

