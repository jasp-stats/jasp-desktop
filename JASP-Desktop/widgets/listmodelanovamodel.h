#ifndef LISTMODELANOVAMODEL_H
#define LISTMODELANOVAMODEL_H

#include <QAbstractListModel>

#include "listmodelvariablesavailable.h"
#include "enhanceddroptarget.h"
#include "options/optionfields.h"

#include "tablemodel.h"

class ListModelAnovaModel : public TableModel, public EnhancedDropTarget, public BoundModel
{
	Q_OBJECT

	friend class AnovaModelWidget;

	enum AssignType { Cross = 0, MainEffects, Interaction, All2Way, All3Way, All4Way, All5Way };

public:
	ListModelAnovaModel(QObject *parent = 0);

	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual int rowCount(const QModelIndex &) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;

	virtual QStringList mimeTypes() const OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int assignType) OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;

	void setVariables(const QList<ColumnInfo> &variables);
	const QList<ColumnInfo> &variables() const;
	void setDependent(const ColumnInfo dependent);
	void setCustomModelMode(bool on);

	virtual void bindTo(Option *option) OVERRIDE;

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

signals:
	void variablesAvailableChanged();

protected:
	static QList<QList<ColumnInfo> > generateCrossCombinations(const QVector<ColumnInfo> &variables);
	static QList<QList<ColumnInfo> > generateWayCombinations(const QVector<ColumnInfo> &variables, int ways);
	static QString itemsToString(QList<ColumnInfo> items);

	OptionFields *_boundTo;

	bool _customModel;

	ColumnInfo _dependent;
	QList<ColumnInfo> _variables;

	QList<QList<ColumnInfo> > _terms;

	void assignToOption();


};

#endif // LISTMODELANOVAMODEL_H
