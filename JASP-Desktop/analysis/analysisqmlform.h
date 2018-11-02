#ifndef ANALYSISQMLFORM_H
#define ANALYSISQMLFORM_H

#include <QWidget>
#include <QQuickItem>
#include <QMessageBox>
#include <QQuickWidget>
#include <QFileSystemWatcher>

#include "analysisform.h"
#include "analysis.h"
#include "boundqmlitem.h"
#include "widgets/listmodel.h"
#include "options/variableinfo.h"
#include "analysisqmldefines.h"
#include "widgets/listmodeltermsavailable.h"

class ListModelTermsAssigned;
class BoundQMLItem;

class AnalysisQMLForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit	AnalysisQMLForm(QWidget *parent, Analysis* analysis);
				~AnalysisQMLForm() {}

	void		bindTo(Options *options, DataSet *dataSet)	OVERRIDE;
	void		unbind()									OVERRIDE;
	
	QWidget*	getWidget()									OVERRIDE	{ return _quickWidget; }
	
	void		addError(const QString& error);

	ListModel*	getRelatedModel(QMLListView* listView)	{ return _relatedModelMap[listView]; }
	ListModel*	getModel(const QString& modelName)		{ return _modelMap[modelName]; }
	Options*	getAnalysisOptions()					{ return _analysis->options(); }
	BoundQMLItem* getBoundItem(const QString& name)		{ return _boundItems[name]; }

public slots:
	void		sceneGraphErrorHandler(QQuickWindow::SceneGraphError error, QString message)	{ QMessageBox::warning(this, "Error", "Error when painting analysis form: " + message); }
	void		statusChangedWidgetHandler(QQuickWidget::Status status);

protected:
	void		_setAllAvailableVariablesModel();
	QString		_getAnalysisQMLPath();
	virtual	void rScriptDoneHandler(QVariant key, const QString & result) OVERRIDE;
	
private:
	void		_parseQML();
	void		_setErrorMessages();

private slots:
	void		QMLFileModifiedHandler(QString path);
	void		RFileModifiedHandler(QString path)						{ qDebug() << "Test R file (" << path << ") modified"; }

protected:	
	QQuickWidget							*_quickWidget;
	Analysis								*_analysis;
	QMap<QString, BoundQMLItem* >			_boundItems;
	std::map<QMLListView*, ListModel* >		_relatedModelMap;
	std::vector<ListModelTermsAvailable* >	_availableVariablesModels;
	std::map<QString, ListModel* >			_modelMap;

private:
	QFileSystemWatcher						_QMLwatcher;
	QFileSystemWatcher						_Rwatcher;
	
	std::vector<ListModelTermsAvailable*>	_allAvailableVariablesModels;
	QQuickItem								*_errorMessagesItem;
	QList<QString>							_errorMessages;	
};

#endif // ANALYSISQMLFORM_H
