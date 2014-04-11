#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include <QMainWindow>
#include "dataset.h"

#include "datasettablemodel.h"
#include "enginesync.h"
#include "analyses.h"
#include "widgets/progresswidget.h"

#include "analysisforms/analysisform.h"
#include "asyncloader.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT
    
public:
	explicit MainWindow(QWidget *parent = 0);
	void open(QString filename);
	~MainWindow();
    
protected:
	virtual void resizeEvent(QResizeEvent *event) OVERRIDE;

private:
	Ui::MainWindow *ui;

	AnalysisForm *_currentOptionsWidget;
	DataSet *_dataSet;
	DataSetTableModel *_tableModel;
	Analysis *_currentAnalysis;

	Analyses *_analyses;
	EngineSync* _engineSync;

	void analysisResultsChangedHandler(Analysis* analysis);

	AsyncLoader _loader;
	ProgressWidget *_alert;

	bool _inited;

	AnalysisForm* loadForm(Analysis *analysis);
	void showForm(Analysis *analysis);

	QWidget *_buttonPanel;
	QVBoxLayout *_buttonPanelLayout;
	QPushButton *_okButton;
	QPushButton *_removeButton;

	std::map<std::string, AnalysisForm *> _analysisForms;

signals:
	void analysisSelected(int id);
	void analysisUnselected();
	void analysisChangedDownstream(int id, QString options);
	void pushToClipboard(QString data);

private slots:

	void analysisSelectedHandler(int id);
	void analysisUnselectedHandler();
	void pushToClipboardHandler(QString data);
	void analysisChangedDownstreamHandler(int id, QString options);

    void tabChanged(int index);
	void dataSetSelected(const QString &filename);
	void dataSetCloseRequested();
	void dataSetLoaded(DataSet *dataSet);
	void itemSelected(const QString item);

	void adjustOptionsPanelWidth();
	void splitterMovedHandler(int, int);
	void repositionButtonPanel(int parentWidth = -1);

	void analysisOKed();
	void analysisRemoved();

	void updateMenuEnabledDisabledStatus();
};

#endif // MAINWIDGET_H
