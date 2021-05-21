#ifndef LABELMODEL_H
#define LABELMODEL_H


#include "datasettableproxy.h"
#include <QTimer>

class LabelModel : public DataSetTableProxy
{
	Q_OBJECT

	Q_PROPERTY(int		filteredOut		READ filteredOut									NOTIFY filteredOutChanged		)
	Q_PROPERTY(int		chosenColumn	READ proxyParentColumn	WRITE setProxyParentColumn	NOTIFY proxyParentColumnChanged	)
	Q_PROPERTY(bool		visible			READ visible			WRITE setVisible			NOTIFY visibleChanged			)
	Q_PROPERTY(QString	columnName		READ columnNameQ		WRITE setColumnNameQ		NOTIFY columnNameChanged		)
	Q_PROPERTY(double	rowWidth		READ rowWidth			WRITE setRowWidth			NOTIFY rowWidthChanged			)
	Q_PROPERTY(double	valueMaxWidth	READ valueMaxWidth									NOTIFY valueMaxWidthChanged		)
	Q_PROPERTY(double	labelMaxWidth	READ labelMaxWidth									NOTIFY labelMaxWidthChanged		)

public:
				LabelModel();

	bool		labelNeedsFilter(size_t col);
	std::string columnName(size_t col);
	QString		columnNameQ();
	bool		setData(const QModelIndex & index, const QVariant & value,	int role = -1)						override;
	QVariant	data(	const QModelIndex & index,							int role = Qt::DisplayRole)	const	override;
	QVariant	headerData(int section, Qt::Orientation orientation, int role)							const	override;

	bool		visible()			const {	return _visible; }
	int			filteredOut()		const;
	int			dataColumnCount()	const;

	Q_INVOKABLE void reverse();
	Q_INVOKABLE void moveSelectionUp();
	Q_INVOKABLE void moveSelectionDown();
	Q_INVOKABLE void resetFilterAllows();
	Q_INVOKABLE void unselectAll();
	Q_INVOKABLE bool setChecked(int rowIndex, bool checked);
	Q_INVOKABLE void setLabel(int rowIndex, QString label);

	std::vector<bool>			filterAllows(size_t col);
	std::vector<std::string>	labels(size_t col);

	double rowWidth()			const	{ return _rowWidth;			}
	double valueMaxWidth()		const	{ return _valueMaxWidth;	}
	double labelMaxWidth()		const	{ return _labelMaxWidth;	}

public slots:
	void filteredOutChangedHandler(int col);
	void setVisible(bool visible);
	void setSelected(int row, int modifier);
	void setColumnNameQ(QString newColumnName);
	void removeAllSelected();
	void columnAboutToBeRemoved(int column);
	void columnDataTypeChanged(const QString & colName);
	void setRowWidth(double len);
	void onChosenColumnChanged();
	void refresh();

signals:
	void visibleChanged(bool visible);
	void filteredOutChanged();
	void columnNameChanged();
	void allFiltersReset();
	void labelFilterChanged();
	void rowWidthChanged();
	void valueMaxWidthChanged();
	void labelMaxWidthChanged();

private:
	std::vector<size_t> getSortedSelection()					const;
	void				setValueMaxWidth();
	void				setLabelMaxWidth();

private:
	bool				_visible		= false;
	double				_valueMaxWidth	= 10,
						_labelMaxWidth	= 10,
						_rowWidth		= 60;
	std::set<QString>	_selected;
	int					_lastSelected	= -1;
};

#endif // LABELMODEL_H
