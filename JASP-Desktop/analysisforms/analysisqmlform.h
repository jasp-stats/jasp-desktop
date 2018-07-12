#ifndef ANALYSISQMLFORM_H
#define ANALYSISQMLFORM_H

#include <QWidget>
#include <QQuickItem>
#include <QQuickWidget>
#include <QFileSystemWatcher>

#include "analysisform.h"
#include "analysis.h"
#include "boundqmlitem.h"
#include "widgets/listmodel.h"
#include "variableinfo.h"

class ListModelTermsAvailable;
class ListModelTermsAssigned;
class BoundQMLItem;
class ListModel;

class AnalysisQMLForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit AnalysisQMLForm(QWidget *parent, Analysis* analysis);
	~AnalysisQMLForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;
	virtual void unbind() OVERRIDE;
	
	virtual QWidget* getWidget() OVERRIDE;
	
	void addError(const QString& error);
	
	ListModel* getRelatedModel(QQuickItem* model);
	ListModel* getModel(const QString& model);
	Options* getAnalysisOptions();
	
public slots:
	void sceneGraphErrorHandler(QQuickWindow::SceneGraphError error, QString message);
	void statusChangedWidgetHandler(QQuickWidget::Status status);

protected:	
	QQuickWidget *_quickWidget;
	Analysis *_analysis;
	std::list<BoundQMLItem* > _items;
	std::map<QQuickItem*, ListModel* > _relatedModel;
	std::vector<ListModelTermsAvailable* > _availableVariablesModels;
	std::map<QString, ListModel*> _modelMap;
	
	void _setAllAvailableVariablesModel();
	
	QString _getAnalysisQMLPath();
	
private slots:
	void QMLFileModifiedHandler(QString path);
	void RFileModifiedHandler(QString path);
	
	
private:
	void _parseQML();
	void _setErrorMessages();
	
	QFileSystemWatcher _QMLwatcher;
	QFileSystemWatcher _Rwatcher;
	
	QQuickItem *_errorMessagesItem;	
	QList<QString> _errorMessages;
	
	ListModelTermsAvailable* _allAvailableVariablesModel;
	
};

#endif // ANALYSISQMLFORM_H
