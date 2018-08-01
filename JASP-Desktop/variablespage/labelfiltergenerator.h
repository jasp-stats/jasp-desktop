#ifndef LABELFILTERGENERATOR_H
#define LABELFILTERGENERATOR_H

#include <QObject>
#include "data/datasetpackage.h"

class labelFilterGenerator : public QObject
{
	Q_OBJECT

public:
	labelFilterGenerator(DataSetPackage *package, QObject *parent = NULL);

	///Generates entire filter
	std::string generateFilter();

public slots:
	void labelFilterChanged();
	void easyFilterConstructorRCodeChanged(QString newRScript);

private:
	///Is at least one label no longer allowed?
	bool		labelNeedsFilter(Column & column) { return !column.allLabelsPassFilter(); }

	///Generates sub-filter for specified column
	std::string	generateLabelFilter(Column & column);

	std::string easyFilterConstructorRScript = "";


	DataSetPackage * _package = NULL;

signals:
	void setGeneratedFilter(QString generatedFilter);


};

#endif // LABELFILTERGENERATOR_H
