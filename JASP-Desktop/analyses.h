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

	Analysis *create(const QString &name, Json::Value *options = NULL, Analysis::Status status = Analysis::Empty);
	Analysis *create(const QString &name, int id, Json::Value *options = NULL, Analysis::Status status = Analysis::Empty);
	Analysis *get(int id) const;
	void clear();

	typedef QList<Analysis*>::iterator iterator;
	iterator begin();
	iterator end();

	int count() const;

signals:
	void analysisInitialised(Analysis *source);
	void analysisOptionsChanged(Analysis *source);
	void analysisResultsChanged(Analysis *source);
	void analysisNotesLoaded(Analysis *source);
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
	void analysisNotesLoadedHandler(Analysis *analysis);

	QList<Analysis*> _analyses;

	int _nextId;

	QMap<QString, Defaults> _defaults;



};


#endif // ANALYSES_H
