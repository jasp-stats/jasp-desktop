#ifndef ANOVAMODELWIDGET_H
#define ANOVAMODELWIDGET_H

#include <QWidget>
#include <QStringListModel>

#include "bound.h"

#include "options/optionstring.h"
#include "options/optionfields.h"
#include "availablefields.h"

#include "listmodelvariablesavailable.h"
#include "listmodelanovamodel.h"

namespace Ui {
class AnovaModelWidget;
}

class AnovaModelWidget : public QWidget, public Bound
{
	Q_OBJECT
	
public:
	explicit AnovaModelWidget(QWidget *parent = 0);
	~AnovaModelWidget();

	virtual void bindTo(Option *option) override;

	virtual void setModel(ListModelAnovaModel *model);

	void setDependent(const QString dependent);
	
private slots:

	void setCustomModelMode(bool customModel);

	void variablesAvailableChanged();
	void sourceSelectionChanged();

	void assignInteraction();
	void assignMainEffects();
	void assign2ways();
	void assign3ways();
	void assign4ways();
	void assign5ways();

private:

	bool _customModel;

	QAction *_assignInteraction;
	QAction *_assignMainEffects;
	QAction *_assign2ways;
	QAction *_assign3ways;
	QAction *_assign4ways;
	QAction *_assign5ways;

	Ui::AnovaModelWidget *ui;

	OptionString *_boundTo;

	ListModelVariablesAvailable *_listModelVariablesAvailable;
	ListModelAnovaModel *_listModelAnovaModel;

};

#endif // ANOVAMODELWIDGET_H
