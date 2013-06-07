#ifndef AVAILABLEFIELDS_H
#define AVAILABLEFIELDS_H

#include <QStringListModel>
#include <QStringList>

#include "dataset.h"
#include "options/optionfields.h"

class AvailableFields : public QStringListModel
{
public:
	AvailableFields(QObject *parent);

	void setDataSet(DataSet *dataSet);

	void provideFor(OptionFields *option);
	QStringList getFields(QModelIndexList indices);

	boost::signals2::signal<void ()> availableFieldsChanged();

private:
	DataSet *_dataSet;
	vector<OptionFields*> _provideFor;
	void updateAvailableFields();

};

#endif // AVAILABLEFIELDS_H
