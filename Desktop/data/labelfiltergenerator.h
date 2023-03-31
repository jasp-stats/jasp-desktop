#ifndef LABELFILTERGENERATOR_H
#define LABELFILTERGENERATOR_H

#include <QObject>
#include "data/labelmodel.h"

///
/// This is used to generate R-filters based on what the user disables/enables in the label-editor (or variableswindow)
class labelFilterGenerator : public QObject
{
	Q_OBJECT

public:
	labelFilterGenerator(LabelModel *labelModel, QObject *parent = NULL);

	///Generates entire filter
	std::string generateFilter();

	void regenerateFilter()	{ emit setGeneratedFilter(QString::fromStdString(generateFilter())); }

public slots:
	void labelFilterChanged();
	void easyFilterConstructorRCodeChanged(QString newRScript);

private:
	///Is at least one label no longer allowed?
	bool		labelNeedsFilter(Column & column) { return !column.allLabelsPassFilter(); }

	///Generates sub-filter for specified column
	std::string	generateLabelFilter(size_t col);

	std::string easyFilterConstructorRScript = "";


	LabelModel * _labelModel = nullptr;

signals:
	void setGeneratedFilter(QString generatedFilter);


};

#endif // LABELFILTERGENERATOR_H
