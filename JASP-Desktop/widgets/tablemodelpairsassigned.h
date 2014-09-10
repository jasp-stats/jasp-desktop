#ifndef TABLEMODELPAIRSASSIGNED_H
#define TABLEMODELPAIRSASSIGNED_H

#include <QAbstractListModel>

#include "options/optionvariablesgroups.h"
#include "tablemodelvariables.h"
#include "tablemodel.h"
#include "droptarget.h"
#include "tablemodelvariablesavailable.h"

class TableModelPairsAssigned : public TableModel, public BoundModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	explicit TableModelPairsAssigned(QObject *parent = 0);

	void setVariableTypesSuggested(int variableTypesSuggested);
	int variableTypesSuggested() const;

	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed() const;

	void bindTo(Option *option) OVERRIDE;
	int rowCount(const QModelIndex &parent) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;
	virtual QStringList mimeTypes() const OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

	void setSource(TableModelVariablesAvailable *source);

protected:

	bool isAllowed(const Term &term) const;
	void assignToOption();

private:
	int _variableTypesAllowed;
	int _variableTypesSuggested;

	TableModelVariablesAvailable *_source;

	OptionVariablesGroups *_boundTo;
	QList<QList<QString> > _values;

	void pairsChanged();
	
};

#endif // TABLEMODELPAIRSASSIGNED_H
