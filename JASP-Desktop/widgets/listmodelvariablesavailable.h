#ifndef LISTMODELVARIABLESAVAILABLE_H
#define LISTMODELVARIABLESAVAILABLE_H

#include "listmodelvariables.h"

#include <QList>
#include <QMimeData>

#include "common.h"

class ListModelVariablesAvailable : public ListModelVariables
{
	Q_OBJECT
public:
	explicit ListModelVariablesAvailable(QObject *parent = 0);

	void setVariables(const QList<ColumnInfo> &variables);
	bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent);
    bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
    virtual QStringList mimeTypes() const OVERRIDE;

	const QList<ColumnInfo> &allVariables() const;

	void notifyAlreadyAssigned(const QList<ColumnInfo> &variables);
    bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;

signals:
	void variablesChanged();

public slots:
	void sendBack(QList<ColumnInfo> &variables);

protected:
	void resort();

private:
	QList<ColumnInfo> _allVariables;

};

#endif // LISTMODELVARIABLESAVAILABLE_H
