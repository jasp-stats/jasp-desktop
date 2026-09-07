
#ifndef COLUMN_MODEL_H
#define COLUMN_MODEL_H


#include <QIdentityProxyModel>
#include "columntype.h"
#include "undostack.h"
#include <QTimer>

class Column;
class DataSet;

/// 
/// This pipes through the label-information for a single column from DataSetPackage
/// The column is selected by changing `proxyParentColumn` from DataSetTableProxy
class ColumnModel : public QIdentityProxyModel
{
	Q_OBJECT

    Q_PROPERTY(int			filteredOut					READ filteredOut                                                NOTIFY filteredOutChanged				)
	Q_PROPERTY(Column *		column						READ column														NOTIFY chosenColumnChanged				)
	Q_PROPERTY(int			chosenColumn				READ chosenColumn				WRITE setChosenColumn			NOTIFY chosenColumnChanged				)
    Q_PROPERTY(bool			visible						READ visible                    WRITE setVisible                NOTIFY visibleChanged					)
	Q_PROPERTY(double		rowWidth					READ rowWidth					WRITE setRowWidth				NOTIFY rowWidthChanged					)
	Q_PROPERTY(double		valueMaxWidth				READ valueMaxWidth												NOTIFY valueMaxWidthChanged				)
	Q_PROPERTY(double		labelMaxWidth				READ labelMaxWidth												NOTIFY labelMaxWidthChanged				)
	Q_PROPERTY(bool			columnIsFiltered			READ columnIsFiltered											NOTIFY columnIsFilteredChanged			)
	Q_PROPERTY(bool			nameEditable				READ nameEditable												NOTIFY nameEditableChanged				)
	Q_PROPERTY(QString		computedType				READ computedType				WRITE setComputedType			NOTIFY computedTypeChanged				)
	Q_PROPERTY(bool			computedTypeEditable		READ computedTypeEditable										NOTIFY computedTypeEditableChanged		)
	Q_PROPERTY(QVariantList	computedTypeValues			READ computedTypeValues											NOTIFY computedTypeValuesChanged		)
	Q_PROPERTY(QString		currentColumnType			READ currentColumnType			WRITE setColumnType				NOTIFY columnTypeChanged				)
	Q_PROPERTY(QVariantList	columnTypeValues			READ columnTypeValues											NOTIFY columnTypeValuesChanged			)
	Q_PROPERTY(bool			useCustomEmptyValues		READ useCustomEmptyValues		WRITE setUseCustomEmptyValues	NOTIFY useCustomEmptyValuesChanged		)
    Q_PROPERTY(QStringList	emptyValues					READ emptyValues                WRITE setCustomEmptyValues		NOTIFY emptyValuesChanged				)
	Q_PROPERTY(QVariantList	tabs						READ tabs														NOTIFY tabsChanged						)
    Q_PROPERTY(bool         isVirtual					READ isVirtual													NOTIFY isVirtualChanged					)
    Q_PROPERTY(bool			compactMode					READ compactMode                WRITE setCompactMode            NOTIFY compactModeChanged				)
    Q_PROPERTY(bool			hasSeveralNumericValues		READ hasSeveralNumericValues                                    NOTIFY hasSeveralNumericValuesChanged	) //Only works when autosort is on
	Q_PROPERTY(int			rowsTotal					READ rowsTotal													NOTIFY rowsTotalChanged					)
    Q_PROPERTY(QString		dropLevels					READ dropLevels					WRITE setDropLevels				NOTIFY dropLevelsChanged                )
	
	

public:
	ColumnModel();
	
	ColumnModel(const ColumnModel &) = delete;
	ColumnModel(ColumnModel &&) = delete;
	ColumnModel &operator=(const ColumnModel &) = delete;
	ColumnModel &operator=(ColumnModel &&) = delete;
	static QVariant columnTypeFriendlyMapping(computedColumnType compColT);
	
	bool			labelNeedsFilter(size_t col);
	QString			columnNameQ();
	QString			columnTitle()					const;
	QString			columnDescription()				const;
	QString			computedType()					const;
	bool			computedTypeEditable()			const;
	bool			isComputed()					const;
	QVariantList	computedTypeValues()			const;
	QString			currentColumnType()				const;
	QVariantList	columnTypeValues()				const;
	bool			useCustomEmptyValues()			const;
	QStringList		emptyValues()					const;
	bool			hasSeveralNumericValues()		const;
	int				rowsTotal()						const;
	QString			dropLevels()					const;
	bool			autoSort()						const;
	QString			computeFilter()					const;


	bool			setData(const QModelIndex & index, const QVariant & value,	int role = Qt::EditRole)			override;
	QVariant		data(	const QModelIndex & index,							int role = Qt::DisplayRole)	const	override;
	QVariant		headerData(int section, Qt::Orientation orientation, int role)							const	override;
	int				rowCount(const QModelIndex & parent = QModelIndex())								const	override;
	//int				columnCount(const QModelIndex & = QModelIndex())										const	override;

	bool			visible()			const {	return _visible; }
	int				filteredOut()		const;
	int				chosenColumn()		const;
	Column *		column()			const;
	bool			nameEditable()		const;
	
	Q_INVOKABLE void reverse();
	Q_INVOKABLE void reverseValues();
	Q_INVOKABLE void toggleAutoSortByValues();
	Q_INVOKABLE void moveSelectionUp();
	Q_INVOKABLE void moveSelectionDown();
	Q_INVOKABLE void resetFilterAllows();
	Q_INVOKABLE void unselectAll();
	Q_INVOKABLE bool setChecked(int rowIndex, bool checked);
	Q_INVOKABLE void setValue(int rowIndex, const QString & value);
	Q_INVOKABLE void setLabel(int rowIndex, QString label);
	Q_INVOKABLE void deleteLabel(int rowIndex);
	Q_INVOKABLE void addLabel(QString value, QString label); ///< Via UndoStack
	Q_INVOKABLE void addEmptyValue(		const QString & value);
	Q_INVOKABLE void removeEmptyValue(	const QString & value);
	Q_INVOKABLE void resetEmptyValues();
	Q_INVOKABLE void undo()				{ if (undoStack()) undoStack()->undo(); }
	Q_INVOKABLE void redo()				{ if (undoStack()) undoStack()->redo(); }
	
	Q_INVOKABLE bool isColumnNameFree(		const QString & name);
	Q_INVOKABLE void createComputedColumn(	const QString & name, int columnType, bool useJsonConstructor);
	
	UndoStack *	undoStack();

	double rowWidth()			const	{ return _rowWidth;			}
	double valueMaxWidth()		const	{ return _valueMaxWidth;	}
	double labelMaxWidth()		const	{ return _labelMaxWidth;	}

	void setColumnTitle(			const QString &		newColumnTitle);
	void setColumnDescription(		const QString &		newColumnDescription);
	void setComputedType(			QString				computedType);
	void setColumnType(				QString				type);
	
	Q_INVOKABLE void setColumnTitleQ(			const QString &		newColumnTitle)				{ setColumnTitle(newColumnTitle);			}
	Q_INVOKABLE void setColumnDescriptionQ(		const QString &		newColumnDescription)		{ setColumnDescription(newColumnDescription);	}
	Q_INVOKABLE void setColumnNameByQString(	const QString &		newColumnName)				{ setColumnNameQ(newColumnName);				}
	Q_INVOKABLE void setHasLabelsQ(				bool				newHasLabels)				{ setHasLabels(newHasLabels);					}
	Q_INVOKABLE void setAutoSortQ(				bool				newAutoSort)				{ setAutoSort(newAutoSort);					}
	Q_INVOKABLE void setComputeFilterQ(			const QString &		newComputeFilter)			{ setComputeFilter(newComputeFilter);			}
	Q_INVOKABLE void setDropLevelsQ(			QString				dropLevels)					{ setDropLevels(dropLevels);					}
	
	void setUseCustomEmptyValues(	bool				useCustomMissingValues);
	void setCustomEmptyValues(		const QStringList&	customMissingValues);
	void setDropLevels(				QString				dropLevels);
	void setAutoSort(				bool				newAutoSort);
	void setComputeFilter(			const QString &		newComputeFilter);

	QVariantList tabs()		const;

	bool columnIsFiltered() const;
	bool isVirtual()		const	{ return _virtual; }
	bool compactMode()		const;
	
	
	
	bool hasLabels() const;
	void setHasLabels(bool newHasLabels);
	
public slots:
	void 		refreshFilteredOut();
	void 		setVisible(bool visible);
	void 		setChosenColumn(int chosenColumn);
	void 		setChosenColumnByName(const QString chosenName, int colIndex=-1);
	void 		setSelected(int row, int modifier);
	void 		setColumnNameQ(QString newColumnName);
	void 		removeAllSelected();
	void 		setRowWidth(double len);
	void 		refresh();
	void 		checkRemovedColumns(int columnIndex, int count);
	void 		checkInsertedColumns(const QModelIndex & parent, int first, int last);
	void 		openComputedColumn(const QString name);
	void 		checkCurrentColumn( int dataSetId, QStringList changedColumns, QStringList missingColumns, QMap<QString, QString>	changeNameColumns, bool rowCountChanged, bool hasNewColumns);
	void 		shownDataSetChangedHandler(DataSet * newDataSet);
	void 		setCompactMode(bool newCompactMode);
	void 		languageChangedHandler();
	void 		setLabelMaxWidth();

signals:
	void 		visibleChanged(bool visible);
	void 		filteredOutChanged();
	void 		columnNameChanged();
	void 		allFiltersReset();
	void 		rowWidthChanged();
	void 		dropLevelsChanged();
	void 		valueMaxWidthChanged();
	void 		columnDescriptionChanged();
	void 		labelMaxWidthChanged();
	void 		chosenColumnChanged();
	void 		columnTitleChanged();
	void 		computedTypeChanged();
	void 		isComputedChanged();
	void		hasLabelsChanged();
	void 		computedTypeEditableChanged();
	void 		computedTypeValuesChanged();
	void 		columnTypeValuesChanged();
	void 		columnTypeChanged();
	void 		columnIsFilteredChanged();
	void 		beforeChangingColumn(QString chosenName);
	void 		nameEditableChanged();
	void 		tabsChanged();
	void 		useCustomEmptyValuesChanged();
	void 		emptyValuesChanged();
	void 		rowsTotalChanged();
	void 		isVirtualChanged();
	void 		compactModeChanged();
	void 		autoSortChanged();
	void 		hasSeveralNumericValuesChanged();
	void 		computeFilterChanged();
	QString 	columnNameForIndex(int index);

	
private:
	std::vector<size_t>		getSortedSelection()					const;
	void					setValueMaxWidth();
	void					clearVirtual();
	// Fires the notify signals of the (GUI-side) properties that depend on the chosen column,
	// as well as chosenColumnChanged which drives the `column` Q_PROPERTY.
	void					notifyColumnChanged();

	struct
	{
		QString				name, title, description, computeFilter;
		columnType			type = columnType::scale;
		computedColumnType	computedType = computedColumnType::notComputed;
	} _dummyColumn;

	bool					_visible			= false,
							_editing			= false,
							_virtual			= false,
							_compactMode		= false,
							_beingRefreshed		= false;
	double					_valueMaxWidth		= 10,
							_labelMaxWidth		= 10,
							_rowWidth			= 60;
	std::set<QString>		_selected;
	int						_lastSelected		= -1;
	QPointer<Column>		_column				= nullptr;
	DataSet				*	_shownDataSet		= nullptr;
	int						_columnIndex		= -1;
};

#endif // COLUMN_MODEL_H
