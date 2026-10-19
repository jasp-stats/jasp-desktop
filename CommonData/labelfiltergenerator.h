#ifndef LABELFILTERGENERATOR_H
#define LABELFILTERGENERATOR_H

#include <QObject>

class DataSet;
class Filter;
///
/// This is used to generate R-filters based on what the user disables/enables in the label-editor (or variableswindow)
class LabelFilterGenerator : public QObject
{
	Q_OBJECT

private:
				LabelFilterGenerator(Filter * filter);
	friend class Filter;
public:
	std::string generateFilter();

public slots:
	void		regenerateGeneratedFilter();

private:
	Filter *	_filter = nullptr;
};

#endif // LABELFILTERGENERATOR_H
