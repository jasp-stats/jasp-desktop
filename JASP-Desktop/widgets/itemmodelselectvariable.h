#ifndef ITEMMODELSELECTVARIABLE_H
#define ITEMMODELSELECTVARIABLE_H

#include <QAbstractListModel>
#include "boundmodel.h"
#include "common.h"
#include "options/optionvariable.h"
#include "widgets/tablemodelvariablesavailable.h"
#include "variableinfo.h"

typedef QPair<QString, int> ColumnInfo;

class ItemModelSelectVariable : public QAbstractListModel, public BoundModel
{
	Q_OBJECT
public:
	explicit ItemModelSelectVariable(QObject *parent = 0);

	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;

	void bindTo(Option* option) OVERRIDE;

	void setSource(TableModelVariablesAvailable *source);

private slots:
	void variablesChangedHandler();

private:

	void updateSelected();

	int _selectedIndex;
	TableModelVariablesAvailable *_source;
	OptionVariable *_boundTo;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
};

#endif // ITEMMODELSELECTVARIABLE_H
