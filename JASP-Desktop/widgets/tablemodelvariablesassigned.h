#ifndef TABLEMODELVARIABLESASSIGNED_H
#define TABLEMODELVARIABLESASSIGNED_H

#include <QAbstractTableModel>

#include "options/optionfieldpairs.h"
#include "listmodelvariables.h"

typedef QList<ColumnInfo> VarPair;

class TableModelVariablesAssigned : public QAbstractTableModel, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelVariablesAssigned(QObject *parent = 0);
	
	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed();

	void bindTo(Option *option) override;
	int rowCount(const QModelIndex &parent) const override;
	int columnCount(const QModelIndex &parent) const override;
	QVariant data(const QModelIndex &index, int role) const override;
	Qt::ItemFlags flags(const QModelIndex &index) const override;

	virtual Qt::DropActions supportedDropActions() const override;
	virtual Qt::DropActions supportedDragActions() const override;
	virtual QStringList mimeTypes() const override;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const override;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) override;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const override;

	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) override;
	//virtual bool setItemData(const QModelIndex &index, const QMap<int, QVariant> &roles) override;

	bool isForbidden(int variableType) const;

	virtual bool insertRows(int row, int count, const QModelIndex &parent) override;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) override;

signals:
	void focused();
	void selectionUpdated();

private slots:
	void removeEmptyRows();

private:
	QList<int> _rowsToRemove;
	bool _rowRemovalScheduled;

	int _variableTypesAllowed;

	OptionFieldPairs *_boundTo;
	QList<VarPair> _values;

	void pairsChanged();

	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

	static std::vector<std::pair<std::string, std::string> > asVector(QList<VarPair> values);
	
};

#endif // TABLEMODELVARIABLESASSIGNED_H
