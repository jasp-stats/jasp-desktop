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

class ListModelTermsAvailable;
class ListModelTermsAssigned;
class BoundQMLItem;
class ListModel;

class AnalysisQMLForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit	AnalysisQMLForm(QWidget *parent, Analysis* analysis);
				~AnalysisQMLForm() {}

	void		bindTo(Options *options, DataSet *dataSet)	override;
	void		unbind()									override;
	
	QWidget*	getWidget()									override	{ return _quickWidget; }
	
	void		addError(const QString& error);

	ListModel*	getRelatedModel(QQuickItem* model)						{ return _relatedModel[model];}
	ListModel*	getModel(const QString& model)							{ return _modelMap[model]; }
	Options*	getAnalysisOptions()									{ return _analysis->options(); }

public slots:
	void		sceneGraphErrorHandler(QQuickWindow::SceneGraphError error, QString message)	{ QMessageBox::warning(this, "Error", "Error when painting analysis form: " + message); }
	void		statusChangedWidgetHandler(QQuickWidget::Status status);

protected:
	void		_setAllAvailableVariablesModel();
	QString		_getAnalysisQMLPath();

private:
	void		_parseQML();
	void		_setErrorMessages();

private slots:
	void		QMLFileModifiedHandler(QString path);
	void		RFileModifiedHandler(QString path)						{ qDebug() << "Test R file (" << path << ") modified"; }

protected:	
	QQuickWidget							*_quickWidget;
	Analysis								*_analysis;
	std::list<BoundQMLItem* >				_items;
	std::map<QQuickItem*, ListModel* >		_relatedModel;
	std::vector<ListModelTermsAvailable* >	_availableVariablesModels;
	std::map<QString, ListModel*>			_modelMap;

private:
	QFileSystemWatcher						_QMLwatcher;
	QFileSystemWatcher						_Rwatcher;
	
	QQuickItem								*_errorMessagesItem;
	QList<QString>							_errorMessages;
	
	ListModelTermsAvailable					*_allAvailableVariablesModel;
};

#endif // ANALYSISQMLFORM_H
