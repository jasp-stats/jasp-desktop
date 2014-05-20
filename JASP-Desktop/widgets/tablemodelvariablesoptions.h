#ifndef TABLEMODELVARIABLESOPTIONS_H
#define TABLEMODELVARIABLESOPTIONS_H

#include <QAbstractTableModel>

#include "boundmodel.h"
#include "options/optionstable.h"
#include "terms.h"

class TableModelVariablesOptions : public QAbstractTableModel, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelVariablesOptions(QObject *parent = 0);

	void bindTo(Option *option) OVERRIDE;

	int rowCount(const QModelIndex &) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	void setVariables(const Terms &variables);
	const Terms& variables() const;

private:

	Terms _variables;
	std::vector<Options*> _rows;
	OptionsTable *_boundTo;

};

#endif // TABLEMODELVARIABLESOPTIONS_H
