#ifndef TABLEMODELANOVAMODELNUISANCEFACTORS_H
#define TABLEMODELANOVAMODELNUISANCEFACTORS_H

#include "tablemodelanovamodel.h"
#include "options/optionvariables.h"

class TableModelAnovaModelNuisanceFactors : public TableModelAnovaModel
{
public:
	TableModelAnovaModelNuisanceFactors(QObject *parent = 0);

	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;	
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	void assignToNuisanceOption();
	void setNuisanceTermsOption(OptionVariables *nuisanceOption);

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

protected:
	QList<bool> _nuisance;
	OptionVariables *_nuisanceOption;
};

#endif // TABLEMODELANOVAMODELNUISANCEFACTORS_H
