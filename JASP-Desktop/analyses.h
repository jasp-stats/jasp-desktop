#ifndef ANALYSES_H
#define ANALYSES_H

#include "analysis.h"

#include <QString>
#include <QMap>
#include <QObject>

class Analyses : public QObject
{
	Q_OBJECT

	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef QMap<int, Analysis *> ById;

public:
	Analyses();

	Analysis *create(const QString &name);
	Analysis *create(const QString &name, int id);
	Analysis *get(int id) const;
	void clear();



	typedef QList<Analysis*>::iterator iterator;
	iterator begin();
	iterator end();

signals:
	void analysisInitialised(Analysis *source);
	void analysisOptionsChanged(Analysis *source);
	void analysisResultsChanged(Analysis *source);
	void analysisAdded(Analysis *source);

private slots:
	void flushDefaultsToDisk();

private:

	typedef struct {
		QString analysisName;
		Options *options;
		bool needsSync;
	} Defaults;

	void assignDefaults(Analysis *analysis);

	void analysisOptionsChangedHandler(Analysis *analysis);
	void analysisResultsChangedHandler(Analysis *analysis);

	QList<Analysis*> _analyses;

	int _nextId;

	QMap<QString, Defaults> _defaults;



};


#endif // ANALYSES_H
