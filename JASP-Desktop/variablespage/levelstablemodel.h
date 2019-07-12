#ifndef LEVELSTABLEMODEL_H
#define LEVELSTABLEMODEL_H

#include <QAbstractTableModel>

#include "column.h"

#include "common.h"
#include "dataset.h"

class LevelsTableModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(int filteredOut READ filteredOut NOTIFY filteredOutChanged)
	Q_PROPERTY(int chosenColumn READ chosenColumn WRITE setChosenColumn NOTIFY chosenColumnChanged)

public:
	LevelsTableModel(QObject *parent = 0);
	~LevelsTableModel()	{ }

	enum class Roles {
		ValueRole = Qt::UserRole + 1,
		LabelRole,
		FilterRole
	};
	Q_ENUM(Roles)

	void setColumn(Column *column);
	Q_INVOKABLE void clearColumn();

	int						rowCount(const QModelIndex &parent = QModelIndex())						const	override;
	int						columnCount(const QModelIndex &parent = QModelIndex())					const	override;
	QVariant				data(const QModelIndex &index, int role)								const	override;
	QVariant				headerData(int section, Qt::Orientation orientation, int role)			const	override;
	Qt::ItemFlags			flags(const QModelIndex &index)											const	override;
	bool					setData(const QModelIndex & index, const QVariant & value, int role)			override;
	QHash<int, QByteArray>	roleNames()																const	override;

	void moveUp(QModelIndexList &selection);
	void moveDown(QModelIndexList &selection);

	Q_INVOKABLE void reverse();
	Q_INVOKABLE void setColumnFromQML()							{ setColumn(&_dataSet->column(_chosenColumn)); }
	Q_INVOKABLE void moveUpFromQML(QVariantList selection)		{ QModelIndexList List = convertQVariantList_to_QModelIndexList(selection); moveUp(List); }
	Q_INVOKABLE void moveDownFromQML(QVariantList selection)	{ QModelIndexList List = convertQVariantList_to_QModelIndexList(selection); moveDown(List); }

	QModelIndexList convertQVariantList_to_QModelIndexList(QVariantList selection);

	Q_INVOKABLE bool setAllowFilterOnLabel(int row, bool newAllowValue);
	Q_INVOKABLE bool allowFilter(int row);
	Q_INVOKABLE void resetFilterAllows();

	void setDataSet(DataSet * thisDataSet);
	int filteredOut();

	int chosenColumn() const { return _chosenColumn; }

public slots:
	void refresh();
	void refreshColumn(Column * column);
	void refreshConnectedModelsToName(Column * column) { emit refreshConnectedModelsByName(column->name());	}


	void setChosenColumn(int chosenColumn);

signals:
	void refreshConnectedModels(Column * column);
	void refreshConnectedModelsByName(std::string columnName);
	void resizeLabelColumn();
	void labelFilterChanged();
	void notifyColumnHasFilterChanged(int column); //should be on column but column is not a Qt object.
	void filteredOutChanged();

	void chosenColumnChanged(int chosenColumn);

private:
	Column		*_column;
	DataSet		*_dataSet		= NULL;
	std::string _colName		= "";
	int			_chosenColumn	= -1;

	void _moveRows(QModelIndexList &selection, bool up = true);
	int currentColumnIndex();

};

#endif // LEVELSTABLEMODEL_H
