#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include <QWidget>
#include "dataset.h"

#include "datasettablemodel.h"
#include "enginesync.h"
#include "analyses.h"

#include "analysisforms/analysisform.h"

namespace Ui {
class MainWidget;
}

class MainWidget : public QWidget
{
    Q_OBJECT
    
public:
    explicit MainWidget(QWidget *parent = 0);
    ~MainWidget();
    
private:
	Ui::MainWidget *ui;

	AnalysisForm *_currentOptionsWidget;
	DataSet *_dataSet;
	DataSetTableModel *_tableModel;

	Analyses *_analyses;
	EngineSync* _engineSync;

	void analysisResultsChangedHandler(Analysis* analysis);

private slots:
    void tabChanged(int index);
    void dataSetLoaded(DataSet *dataSet);
	void itemSelected(const QString item);
	void messageReceived(const QString message);

	void analysisOKed();

};

#endif // MAINWIDGET_H
