#ifndef TABLEMODEL_H
#define TABLEMODEL_H

#include <QAbstractItemModel>

class TableModel : public QAbstractTableModel
{
	Q_OBJECT
public:
	explicit TableModel(QObject *parent = 0);

	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) override;

private slots:
	void removeEmptyRows();

private:

	QList<int> _rowsToRemove;
	bool _rowRemovalScheduled;

};

#endif // TABLEMODEL_H
