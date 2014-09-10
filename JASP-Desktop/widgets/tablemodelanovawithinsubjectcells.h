#ifndef TABLEMODELANOVAWITHINSUBJECTCELLS_H
#define TABLEMODELANOVAWITHINSUBJECTCELLS_H

#include "options/optionvariables.h"
#include "widgets/tablemodel.h"
#include "boundmodel.h"
#include "tablemodelvariablesavailable.h"
#include "variableinfo.h"

#include "common.h"

typedef QPair<QString, QStringList> Factor;

class TableModelAnovaWithinSubjectCells : public TableModelVariables, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelAnovaWithinSubjectCells(QObject *parent = 0);

	void bindTo(Option *option) OVERRIDE;
	void setSource(TableModelVariablesAvailable *source);

	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;
	virtual QStringList mimeTypes() const OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

signals:

public slots:
	void setDesign(const QList<Factor> &design);

private:

	int slotsAvailable() const;
	int designCellCount() const;

	TableModelVariablesAvailable *_source;
	OptionVariables *_boundTo;
	std::vector<std::string> _variables;
	QList<Factor> _design;

	Terms _toSendBack;

private slots:
	void sendBack();

};

#endif // TABLEMODELANOVAWITHINSUBJECTCELLS_H
