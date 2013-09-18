#ifndef LISTMODELVARIABLES_H
#define LISTMODELVARIABLES_H

#include <QAbstractListModel>

#include "boundmodel.h"

#include <vector>
#include <string>

#include <QString>
#include <QPair>
#include <QList>
#include <QIcon>
#include <QAbstractItemView>

typedef QPair<QString, int> ColumnInfo;

class ListModelVariables : public QAbstractListModel
{
	Q_OBJECT
public:
	explicit ListModelVariables(QObject *parent = 0);
	
	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed();

	virtual int rowCount(const QModelIndex &) const override;
	virtual QVariant data(const QModelIndex &index, int role) const override;

	virtual bool removeRows(int row, int count, const QModelIndex &parent) override;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) override;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const override;

	virtual Qt::DropActions supportedDropActions() const override;
	virtual Qt::DropActions supportedDragActions() const override;

	virtual QStringList mimeTypes() const override;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const override;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) override;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const override;

	void setSupportedDropActions(Qt::DropActions actions);
	void setSupportedDragActions(Qt::DropActions actions);

	void setDefaultTarget(QAbstractItemView *defaultTarget);

	void setMimeType(const QString &mimeType);

protected:

	QList<ColumnInfo> _variables;

	bool isForbidden(int variableType) const;
	bool isDroppingToSelf(const QMimeData *mimeData) const;

private:

	QAbstractItemView *_defaultTarget;

	Qt::DropActions _dropActions;
	Qt::DropActions _dragActions;

	int _variableTypesAllowed;
	QMimeData *_mimeData;

	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

	QString _mimeType;
	
};

#endif // LISTMODELVARIABLES_H
