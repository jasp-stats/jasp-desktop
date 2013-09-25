#ifndef LISTMODELVARIABLESAVAILABLE_H
#define LISTMODELVARIABLESAVAILABLE_H

#include "listmodelvariables.h"

#include <QList>
#include <QMimeData>

class ListModelVariablesAvailable : public ListModelVariables
{
	Q_OBJECT
public:
	explicit ListModelVariablesAvailable(QObject *parent = 0);

	void setVariables(const QList<ColumnInfo> &variables);
	bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent);
	bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const override;
	virtual QStringList mimeTypes() const override;

	const QList<ColumnInfo> &allVariables() const;

	void notifyAlreadyAssigned(const QList<ColumnInfo> &variables);

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
