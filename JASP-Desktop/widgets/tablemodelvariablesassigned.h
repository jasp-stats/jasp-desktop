#ifndef TABLEMODELVARIABLESASSIGNED_H
#define TABLEMODELVARIABLESASSIGNED_H

#include <QAbstractListModel>

#include "options/optionfieldpairs.h"
#include "listmodelvariables.h"
#include "tablemodel.h"
#include "droptarget.h"
#include "listmodelvariablesavailable.h"

typedef QList<ColumnInfo> VarPair;

class TableModelVariablesAssigned : public TableModel, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelVariablesAssigned(QObject *parent = 0);
	
	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed();

	void bindTo(Option *option) OVERRIDE;
	int rowCount(const QModelIndex &parent) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;
	virtual QStringList mimeTypes() const OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;

	bool isForbidden(int variableType) const;

	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

	void setSource(ListModelVariablesAvailable *source);
protected:
	void assignToOption();

private:
	int _variableTypesAllowed;

	ListModelVariablesAvailable *_source;

	OptionFieldPairs *_boundTo;
	QList<VarPair> _values;

	void pairsChanged();

	static std::vector<std::pair<std::string, std::string> > asVector(QList<VarPair> values);
	
};

#endif // TABLEMODELVARIABLESASSIGNED_H
