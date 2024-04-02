#ifndef COMPUTEDCOLUMNSCODEITEM_H
#define COMPUTEDCOLUMNSCODEITEM_H

#include <QQuickItem>
#include "datasetpackage.h"

/// 
/// A model for use by the computed columns editor in QML
/// It can only show the relevant information for a single computed column at a time
class ComputedColumnModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		computeColumnUsesRCode		READ computeColumnUsesRCode													NOTIFY computeColumnUsesRCodeChanged	)
	Q_PROPERTY(QString	computeColumnRCode			READ computeColumnRCode				WRITE setComputeColumnRCode				NOTIFY computeColumnRCodeChanged		)
	Q_PROPERTY(bool		computeColumnForceType		READ computeColumnForceType			WRITE setComputeColumnForceType			NOTIFY computeColumnForceTypeChanged	)
	Q_PROPERTY(QString	computeColumnJson			READ computeColumnJson														NOTIFY computeColumnJsonChanged			)
	Q_PROPERTY(QString	computeColumnError			READ computeColumnError														NOTIFY computeColumnErrorChanged		)
	Q_PROPERTY(int		columnType					READ computedColumnColumnType												NOTIFY columnTypeChanged				)
	Q_PROPERTY(bool		datasetLoaded				READ datasetLoaded															NOTIFY refreshProperties				)

public:
    explicit	ComputedColumnModel();

				bool				datasetLoaded()					{ return DataSetPackage::pkg()->hasDataSet();	}
				DataSet			*	dataSet()						{ return DataSetPackage::pkg()->dataSet();		}
				const Columns	&	computedColumns() const;
				QString				computeColumnRCode();
				QString				computeColumnRCodeCommentStripped();
				QString				computeColumnError();
				QString				computeColumnJson();
				int					computedColumnColumnType();
				bool				computeColumnForceType()	const;
				Column			*	column()					const;
				bool				computeColumnUsesRCode();

				void				selectColumn(					Column		  * column);
				void				setComputeColumnRCode(			const QString & newCode);
				void				setComputeColumnJson(			const QString & newJson);
				void				setComputeColumnForceType(bool newComputeColumnForceType);

	Q_INVOKABLE void				sendCode(const QString & code);
	Q_INVOKABLE void				sendCode(const QString & code, const QString & json);

	Q_INVOKABLE	void				removeColumn();
	Q_INVOKABLE bool				isColumnNameFree(const QString & name)						{ return DataSetPackage::pkg()->isColumnNameFree(name.toStdString()); }

				Column			*	createComputedColumn(const std::string & name, int columnType, computedColumnType computeType, Analysis * analysis = nullptr);
	Q_INVOKABLE void				createComputedColumn(const QString     & name, int columnType, bool jsonPlease);

				bool				areLoopDependenciesOk(const std::string & columnName);
				bool				areLoopDependenciesOk(const std::string & columnName, const std::string & code);

	Q_INVOKABLE bool				showAnalysisFormForColumn(const QString & columnName);

                                static		ComputedColumnModel * singleton()		{ return _singleton; }

	
								
								
private:
				void				revertToDefaultInvalidatedColumns();
				void				validate(							const QString		& name);
				void				emitHeaderDataChanged(				const QString		& name);
				void				checkForDependentAnalyses(			const std::string	& columnName);
				void				invalidate(							const QString		& name);
				void				invalidateDependents(				const std::string	& columnName);
				void				emitSendComputeCode(				Column				* column);

signals:
				void	refreshProperties();
				void	computeColumnRCodeChanged();
				void	computeColumnErrorChanged();
				void	computeColumnJsonChanged();
				void	refreshColumn(QString columnName);
				void	headerDataChanged(Qt::Orientation orientation, int first, int last);
				void	sendComputeCode(QString columnName, QString code, enum columnType columnType, bool forceType);
				void	computeColumnUsesRCodeChanged();
				void	showAnalysisForm(Analysis *analysis);
				void	dataColumnAdded(QString columnName);
				void	refreshData();
				void	computeColumnForceTypeChanged();
				void	columnTypeChanged();
				
public slots:
				void	checkForDependentColumnsToBeSent(QString columnName, bool refreshMe = false);
				void	computeColumnSucceeded(QString columnName, QString warning, bool dataChanged);
				void	computeColumnRemoved(QString columnNameQ);
				void	computeColumnFailed(QString columnName, QString error);
				void	checkForDependentColumnsToBeSentSlot(QString columnName)					{ checkForDependentColumnsToBeSent(columnName, false); }
				void	recomputeColumn(QString columnName);
				void	analysisRemoved(Analysis * analysis);
				void	datasetChanged(	QStringList				changedColumns,
										QStringList				missingColumns,
										QMap<QString, QString>	changeNameColumns,
										bool					rowCountChanged,
										bool					hasNewColumns);

private:
	static	ComputedColumnModel		* _singleton;
			Column					* _selectedColumn	= nullptr;

			UndoStack				* _undoStack		= nullptr;

			
			
};

#endif // COMPUTEDCOLUMNSCODEITEM_H

