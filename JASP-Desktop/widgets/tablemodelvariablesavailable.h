#ifndef TABLEMODELVARIABLESAVAILABLE_H
#define TABLEMODELVARIABLESAVAILABLE_H

#include "tablemodelvariables.h"

#include <QList>
#include <QMimeData>

#include "terms.h"
#include "common.h"
#include "variableinfo.h"

class TableModelVariablesAvailable : public TableModelVariables, public VariableInfoProvider
{
	Q_OBJECT
public:
	explicit TableModelVariablesAvailable(QObject *parent = 0);

	void setVariables(const Terms &variables);
    bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
    virtual QStringList mimeTypes() const OVERRIDE;

	const Terms &allVariables() const;

	void notifyAlreadyAssigned(const Terms &variables);
    bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const OVERRIDE;

signals:
	void variablesChanged();

public slots:
	void sendBack(Terms &variables);

private:
	Terms _allVariables;

};

#endif // TABLEMODELVARIABLESAVAILABLE_H
