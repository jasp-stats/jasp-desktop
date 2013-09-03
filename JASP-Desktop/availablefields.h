#ifndef AVAILABLEFIELDS_H
#define AVAILABLEFIELDS_H

#include <QAbstractListModel>
#include <QStringList>
#include <QIcon>

#include <vector>
#include <string>

#include "dataset.h"
#include "options/optionfields.h"

class AvailableFields : public QAbstractListModel
{
public:
	AvailableFields(QObject *parent);

	void setDataSet(DataSet *dataSet);
	void filter(std::vector<std::string> show);

	void provideFor(OptionFields *option);
	QStringList getFields(QModelIndexList indices);

	boost::signals2::signal<void ()> availableFieldsChanged();

	int rowCount(const QModelIndex &) const override;
	QVariant data(const QModelIndex &index, int role) const override;

	QStringList available();

private:
	DataSet *_dataSet;
	std::vector<OptionFields *> _provideFor;
	void updateAvailableFields();
	QStringList _availableFields;

	std::vector<std::string> _filter;
	bool _shouldFilter;

	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

};

#endif // AVAILABLEFIELDS_H
