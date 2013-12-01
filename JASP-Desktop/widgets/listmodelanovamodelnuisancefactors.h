#ifndef LISTMODELANOVAMODELNUISANCEFACTORS_H
#define LISTMODELANOVAMODELNUISANCEFACTORS_H

#include "listmodelanovamodel.h"
#include "options/optionfields.h"

class ListModelAnovaModelNuisanceFactors : public ListModelAnovaModel
{
public:
	ListModelAnovaModelNuisanceFactors(QObject *parent = 0);

	virtual int columnCount(const QModelIndex &parent) const override;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const override;
	virtual QVariant data(const QModelIndex &index, int role) const override;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) override;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) override;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) override;	
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const override;

	void assignToNuisanceOption();
	void setNuisanceTermsOption(OptionFields *nuisanceOption);

	virtual void mimeDataMoved(const QModelIndexList &indexes) override;

protected:
	QList<bool> _nuisance;
	OptionFields *_nuisanceOption;
};

#endif // LISTMODELANOVAMODELNUISANCEFACTORS_H
