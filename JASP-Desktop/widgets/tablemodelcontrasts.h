#ifndef TABLEMODELCONTRASTS_H
#define TABLEMODELCONTRASTS_H

#include "tablemodel.h"
#include "boundmodel.h"
#include "options/options.h"
#include "options/optionstable.h"
#include "terms.h"

class TableModelContrasts : public TableModel, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelContrasts(QObject *parent = 0);

	void bindTo(Option *option) OVERRIDE;
	void setLabels(const Terms &levels);

	int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::DisplayRole) OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

signals:

public slots:

private:
	Terms _labels;
	std::vector<Options *> _contrasts;
	OptionsTable *_boundTo;

};

#endif // TABLEMODELCONTRASTS_H
