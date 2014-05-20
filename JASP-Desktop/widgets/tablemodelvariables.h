#ifndef TABLEMODELVARIABLES_H
#define TABLEMODELVARIABLES_H

#include <QAbstractListModel>

#include "boundmodel.h"

#include <vector>
#include <string>

#include <QString>
#include <QPair>
#include <QList>
#include <QIcon>
#include <QAbstractItemView>

#include "terms.h"
#include "tablemodel.h"
#include "common.h"
#include "variableinfo.h"

class TableModelVariables : public TableModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	explicit TableModelVariables(QObject *parent = 0);
	
	void setVariableTypesSuggested(int variableTypesSuggested);
	int variableTypesSuggested();

	void setIsNominalTextAllowed(bool allowed);
	bool isNominalTextAllowed();

    virtual int rowCount(const QModelIndex &) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;
    virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;

    virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
    virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

    virtual Qt::DropActions supportedDropActions() const OVERRIDE;
    virtual Qt::DropActions supportedDragActions() const OVERRIDE;

    virtual QStringList mimeTypes() const OVERRIDE;
    virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
    virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
    virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;

	void setSupportedDropActions(Qt::DropActions actions);
	void setSupportedDragActions(Qt::DropActions actions);

	void setMimeType(const QString &mimeType);

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

protected:

	Terms _variables;

	bool isForbidden(const Term &term) const;
	bool isSuggested(const Term &term) const;

	bool isDroppingToSelf(const QMimeData *mimeData) const;

	QString _mimeType;

private:

	QAbstractItemView *_defaultTarget;

	Qt::DropActions _dropActions;
	Qt::DropActions _dragActions;

	int _variableTypesSuggested;

	bool _nominalTextAllowed;
	QMimeData *_mimeData;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
	
};

#endif // TABLEMODELVARIABLES_H
