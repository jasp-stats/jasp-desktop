#ifndef LISTMODELVARIABLESASSIGNED_H
#define LISTMODELVARIABLESASSIGNED_H

#include "listmodelvariables.h"

#include "listmodelvariablesavailable.h"
#include "options/optionfields.h"

#include "column.h"

class ListModelVariablesAssigned : public ListModelVariables, public BoundModel
{
	Q_OBJECT
public:
	explicit ListModelVariablesAssigned(QObject *parent = 0);
	virtual void bindTo(Option *option) override;

	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const override;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) override;

	void setSource(ListModelVariablesAvailable *source);

private slots:
	void assignToBoundOption();
	void eject();

private:

	OptionFields *_boundTo;
	bool _onlyOne;

	ListModelVariablesAvailable *_source;

	QList<ColumnInfo> _toEject;

};

#endif // LISTMODELVARIABLESASSIGNED_H
