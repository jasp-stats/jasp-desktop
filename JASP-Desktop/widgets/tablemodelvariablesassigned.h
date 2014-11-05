#ifndef TABLEMODELVARIABLESASSIGNED_H
#define TABLEMODELVARIABLESASSIGNED_H

#include "tablemodelvariables.h"

#include "tablemodelvariablesavailable.h"
#include "options/optionvariables.h"

#include "column.h"

class TableModelVariablesAssigned : public TableModelVariables, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelVariablesAssigned(QObject *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	void setSource(TableModelVariablesAvailable *source);

	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual void mimeDataMoved(const QModelIndexList &indices) OVERRIDE;

	bool setSorted(bool sorted);
	const Terms &assigned() const;

signals:
	void assignmentsChanged();

private slots:
	void sourceVariablesChanged();
	void sendBack();

private:
	void assign(const Terms &variables);
	void unassign(const Terms &variables);
	void setAssigned(const Terms &variables);

	OptionVariables *_boundTo;
	TableModelVariablesAvailable *_source;
	bool _sorted;

	Terms _toSendBack;
};

#endif // TABLEMODELVARIABLESASSIGNED_H
