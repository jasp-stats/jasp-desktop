
#ifndef COLUMN_MODEL_H
#define COLUMN_MODEL_H


#include "datasettableproxy.h"
#include "undostack.h"
#include <QTimer>

/// 
/// This pipes through the label-information for a single column from DataSetPackage
/// The column is selected by changing `proxyParentColumn` from DataSetTableProxy
class ColumnModel : public DataSetTableProxy
{
	Q_OBJECT

	Q_PROPERTY(int		filteredOut			READ filteredOut									NOTIFY filteredOutChanged		)
	Q_PROPERTY(int		chosenColumn		READ chosenColumn		WRITE setChosenColumn		NOTIFY chosenColumnChanged		)
	Q_PROPERTY(bool		visible				READ visible			WRITE setVisible			NOTIFY visibleChanged			)
	Q_PROPERTY(QString	columnName			READ columnNameQ		WRITE setColumnNameQ		NOTIFY columnNameChanged		)
	Q_PROPERTY(QString	columnTitle			READ columnTitle		WRITE setColumnTitle		NOTIFY columnTitleChanged		)
	Q_PROPERTY(QString	columnDescription	READ columnDescription	WRITE setColumnDescription	NOTIFY columnDescriptionChanged	)
	Q_PROPERTY(double	rowWidth			READ rowWidth			WRITE setRowWidth			NOTIFY rowWidthChanged			)
	Q_PROPERTY(double	valueMaxWidth		READ valueMaxWidth									NOTIFY valueMaxWidthChanged		)
	Q_PROPERTY(double	labelMaxWidth		READ labelMaxWidth									NOTIFY labelMaxWidthChanged		)
	Q_PROPERTY(bool		showLabelEditor		READ showLabelEditor								NOTIFY showLabelEditorChanged	)
	Q_PROPERTY(bool		showComputedColumn	READ showComputedColumn 							NOTIFY showComputedColumnChanged)
	Q_PROPERTY(bool		columnIsFiltered	READ columnIsFiltered								NOTIFY columnIsFilteredChanged	)


public:
	ColumnModel();

	bool		labelNeedsFilter(size_t col);
	std::string columnName(size_t col); ///< Not a proxy columnIndex!
	QString		columnNameQ();
	QString		columnTitle() const;
	QString		columnDescription() const;
	bool		setData(const QModelIndex & index, const QVariant & value,	int role = Qt::EditRole)						override;
	QVariant	data(	const QModelIndex & index,							int role = Qt::DisplayRole)	const	override;
	QVariant	headerData(int section, Qt::Orientation orientation, int role)							const	override;

	bool		visible()			const {	return _visible; }
	int			filteredOut()		const;
	int			dataColumnCount()	const;
	int			chosenColumn()		const;
	Column *	column()			const;
	
	Q_INVOKABLE void reverse();
	Q_INVOKABLE void moveSelectionUp();
	Q_INVOKABLE void moveSelectionDown();
	Q_INVOKABLE void resetFilterAllows();
	Q_INVOKABLE void unselectAll();
	Q_INVOKABLE bool setChecked(int rowIndex, bool checked);
	Q_INVOKABLE void setLabel(int rowIndex, QString label);
	Q_INVOKABLE void undo()				{ _undoStack->undo(); }
	Q_INVOKABLE void redo()				{ _undoStack->redo(); }

	boolvec			filterAllows(size_t col);
	stringvec		labels(size_t col);

	double rowWidth()			const	{ return _rowWidth;			}
	double valueMaxWidth()		const	{ return _valueMaxWidth;	}
	double labelMaxWidth()		const	{ return _labelMaxWidth;	}

	void setColumnTitle(const QString & newColumnTitle);

	void setColumnDescription(const QString & newColumnDescription);
	void setLabelMaxWidth();


	bool showLabelEditor() const;
	bool showComputedColumn() const;
	bool columnIsFiltered() const;


public slots:
	void filteredOutChangedHandler(int col);
	void setVisible(bool visible);
	void setChosenColumn(int chosenColumn);
	void setChosenColumn(QString& chosenName);
	void setSelected(int row, int modifier);
	void setColumnNameQ(QString newColumnName);
	void removeAllSelected();
	void columnDataTypeChanged(const QString & colName);
	void setRowWidth(double len);
	void onChosenColumnChanged();
	void refresh();
	void changeSelectedColumn(QPoint selectionStart);
	void checkRemovedColumns(int columnIndex, int count);
	void openComputedColumn(QString name);

signals:
	void visibleChanged(bool visible);
	void filteredOutChanged();
	void columnNameChanged();
	void allFiltersReset();
	void labelFilterChanged();
	void rowWidthChanged();
	void valueMaxWidthChanged();
	void labelMaxWidthChanged();
	void chosenColumnChanged();
	void columnTitleChanged();
	void columnDescriptionChanged();
	void showLabelEditorChanged();
	void showComputedColumnChanged();
	void columnIsFilteredChanged();
	void beforeChangingColumn(int chosenColumn);



private:
	std::vector<size_t> getSortedSelection()					const;
	void				setValueMaxWidth();

private:
	bool				_visible		= false,
						_editing		= false;
	double				_valueMaxWidth	= 10,
						_labelMaxWidth	= 10,
						_rowWidth		= 60;
	std::set<QString>	_selected;
	int					_lastSelected	= -1;
	UndoStack*			_undoStack		= nullptr;
};

#endif // COLUMN_MODEL_H
