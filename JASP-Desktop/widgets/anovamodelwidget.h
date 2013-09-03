#ifndef ANOVAMODELWIDGET_H
#define ANOVAMODELWIDGET_H

#include <QWidget>
#include <QStringListModel>

#include "bound.h"
#include "boundmulti.h"

#include "options/optionstring.h"
#include "options/optionfields.h"
#include "availablefields.h"


namespace Ui {
class AnovaModelWidget;
}

class AnovaModelWidget : public QWidget, public Bound, public BoundMulti
{
	Q_OBJECT
	
public:
	explicit AnovaModelWidget(QWidget *parent = 0);
	~AnovaModelWidget();

	virtual void bindTo(Option *option) override;
	virtual void bindTo(Option *option, int item) override;
	virtual void setDataSet(DataSet *dataSet) override;

	enum BindTo { FIXED_FACTORS, RANDOM_FACTORS, MAIN_EFFECTS, INTERACTIONS };
	
private:

	void optionChangedHandler(Option *option);

	Ui::AnovaModelWidget *ui;

	OptionString *_boundTo;

	AvailableFields _availableFields;

	OptionFields *_optionFixedFactors;
	OptionFields *_optionRandomFactors;

	QStringListModel _mainEffectsModel;
	QStringListModel _interactionsModel;
};

#endif // ANOVAMODELWIDGET_H
