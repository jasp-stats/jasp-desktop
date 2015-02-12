#ifndef TABLEMODELANOVAMODEL_H
#define TABLEMODELANOVAMODEL_H

#include <QAbstractListModel>
#include <list>

#include "tablemodelvariablesavailable.h"
#include "enhanceddroptarget.h"
#include "options/optionvariables.h"
#include "options/optionstable.h"

#include "tablemodel.h"

class TableModelAnovaModel : public TableModel, public EnhancedDropTarget, public BoundModel
{
	Q_OBJECT

	friend class AnovaModelWidget;

	enum AssignType { Cross = 0, MainEffects, Interaction, All2Way, All3Way, All4Way, All5Way };

public:
	TableModelAnovaModel(QObject *parent = 0);

	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) OVERRIDE;

	virtual QStringList mimeTypes() const OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent = QModelIndex()) OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int assignType) OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;

	const Terms &variables() const;

	virtual void bindTo(Option *option) OVERRIDE;

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

	const Terms &terms() const;

public slots:

	void setVariables(const Terms &fixedFactors, const Terms &randomFactors = Terms(), const Terms &covariates = Terms());

	void addFixedFactors(const Terms &terms);
	void addRandomFactors(const Terms &terms);
	void addCovariates(const Terms &terms);
	void removeVariables(const Terms &terms);

signals:
	void variablesAvailableChanged();
	void termsChanged();

protected:

	static OptionVariables* termOptionFromRow(Options *row);

	void setTerms(const Terms &terms);

	void clear();
	void assign(const Terms &terms);

	OptionsTable *_boundTo;

	std::vector<Options *> _rows;

	Terms _variables;

	Terms _covariates;
	Terms _fixedFactors;
	Terms _randomFactors;

	Terms _terms;

};

#endif // TABLEMODELANOVAMODEL_H
