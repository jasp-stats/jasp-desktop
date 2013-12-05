#ifndef LISTMODELANOVAMODELNUISANCEFACTORS_H
#define LISTMODELANOVAMODELNUISANCEFACTORS_H

#include "listmodelanovamodel.h"
#include "options/optionfields.h"

class ListModelAnovaModelNuisanceFactors : public ListModelAnovaModel
{
public:
	ListModelAnovaModelNuisanceFactors(QObject *parent = 0);

	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;	
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	void assignToNuisanceOption();
	void setNuisanceTermsOption(OptionFields *nuisanceOption);

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

protected:
	QList<bool> _nuisance;
	OptionFields *_nuisanceOption;
};

#endif // LISTMODELANOVAMODELNUISANCEFACTORS_H
