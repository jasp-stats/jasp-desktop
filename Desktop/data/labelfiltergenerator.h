#ifndef LABELFILTERGENERATOR_H
#define LABELFILTERGENERATOR_H

#include <QObject>
#include "data/columnmodel.h"

///
/// This is used to generate R-filters based on what the user disables/enables in the label-editor (or variableswindow)
class labelFilterGenerator : public QObject
{
	Q_OBJECT

public:
	labelFilterGenerator(ColumnModel *columnModel, QObject *parent = NULL);

	///Generates entire filter
	std::string generateFilter();

	void regenerateFilter()	{ emit setGeneratedFilter(QString::fromStdString(generateFilter())); }

public slots:
	void labelFilterChanged();
	void easyFilterConstructorRCodeChanged(QString newRScript);

private:
	///Generates sub-filter for specified column
	std::string	generateLabelFilter(size_t col);

	std::string easyFilterConstructorRScript = "";
	
	
	ColumnModel * _columnModel = nullptr;

signals:
	void setGeneratedFilter(QString generatedFilter);


};

#endif // LABELFILTERGENERATOR_H
