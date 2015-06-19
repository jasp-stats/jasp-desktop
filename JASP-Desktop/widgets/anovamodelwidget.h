#ifndef ANOVAMODELWIDGET_H
#define ANOVAMODELWIDGET_H

#include <QWidget>
#include <QStringListModel>

#include "bound.h"

#include "options/optionvariables.h"
#include "availablefields.h"

#include "tablemodelvariablesavailable.h"
#include "tablemodelanovamodel.h"

namespace Ui {
class AnovaModelWidget;
}

class AnovaModelWidget : public QWidget, public Bound
{
	Q_OBJECT
	
public:
	explicit AnovaModelWidget(QWidget *parent = 0);
	~AnovaModelWidget();

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void setModel(TableModelAnovaModel *model);

	void setAssignPiecesVisible(bool visible);
	void setFactorsLabel(const QString &label);
	
private slots:

	void variablesAvailableChanged();
	void sourceSelectionChanged();

	void assignInteraction();
	void assignMainEffects();
	void assign2ways();
	void assign3ways();
	void assign4ways();
	void assign5ways();

private:

	QAction *_assignInteraction;
	QAction *_assignMainEffects;
	QAction *_assign2ways;
	QAction *_assign3ways;
	QAction *_assign4ways;
	QAction *_assign5ways;

	Ui::AnovaModelWidget *ui;

	OptionsTable *_boundTo;

	TableModelVariablesAvailable *_tableModelVariablesAvailable;
	TableModelAnovaModel *_tableModelAnovaModel;

};

#endif // ANOVAMODELWIDGET_H
