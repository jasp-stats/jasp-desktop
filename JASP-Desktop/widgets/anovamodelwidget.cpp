#include "anovamodelwidget.h"
#include "ui_anovamodelwidget.h"

#include <boost/foreach.hpp>
#include <QDebug>

#include "analysisforms/analysisform.h"
#include "draganddrop.h"

using namespace std;

AnovaModelWidget::AnovaModelWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AnovaModelWidget)
{
	ui->setupUi(this);

	_boundTo = NULL;
	_listModelAnovaModel = NULL;

	connect(ui->custom, SIGNAL(toggled(bool)), this, SLOT(setCustomModelMode(bool)));
	setCustomModelMode(false);

	_listModelVariablesAvailable = new ListModelVariablesAvailable(this);
	_listModelVariablesAvailable->setSupportedDragActions(Qt::CopyAction);
	_listModelVariablesAvailable->setSupportedDropActions(Qt::MoveAction);
	_listModelVariablesAvailable->setMimeType("application/vnd.list.term");
	ui->listFactorsAvailable->setModel(_listModelVariablesAvailable);

	ui->listFactorsAvailable->selectionUpdated.connect(boost::bind(&AnovaModelWidget::sourceSelectionChanged, this));

	ui->buttonAssignCross->setSourceAndTarget(ui->listFactorsAvailable, ui->listModelTerms);
	ui->buttonAssignMenu->setSourceAndTarget(ui->listFactorsAvailable, ui->listModelTerms);

	_assignInteraction = new QAction("Interaction", this);
	_assignMainEffects = new QAction("Main Effects", this);
	_assign2ways = new QAction("All 2 way", this);
	_assign3ways = new QAction("All 3 way", this);
	_assign4ways = new QAction("All 4 way", this);
	_assign5ways = new QAction("All 5 way", this);

	connect(_assignInteraction, SIGNAL(triggered()), this, SLOT(assignInteraction()));
	connect(_assignMainEffects, SIGNAL(triggered()), this, SLOT(assignMainEffects()));
	connect(_assign2ways, SIGNAL(triggered()), this, SLOT(assign2ways()));
	connect(_assign3ways, SIGNAL(triggered()), this, SLOT(assign3ways()));
	connect(_assign4ways, SIGNAL(triggered()), this, SLOT(assign4ways()));
	connect(_assign5ways, SIGNAL(triggered()), this, SLOT(assign5ways()));

	QList<QAction *> actions;
	actions.append(_assignInteraction);
	actions.append(_assignMainEffects);
	actions.append(_assign2ways);
	actions.append(_assign3ways);
	actions.append(_assign4ways);
	actions.append(_assign5ways);

	QMenu *menu = new QMenu(ui->buttonAssignMenu);
	menu->addActions(actions);
	ui->buttonAssignMenu->setMenu(menu);
}

AnovaModelWidget::~AnovaModelWidget()
{
	delete ui;
}

void AnovaModelWidget::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFields *>(option);

	if (_listModelAnovaModel != NULL && _boundTo != NULL)
		_listModelAnovaModel->bindTo(_boundTo);
}

void AnovaModelWidget::setModel(ListModelAnovaModel *model)
{
	_listModelAnovaModel = model;

	if (_boundTo != NULL)
		_listModelAnovaModel->bindTo(_boundTo);

	_listModelAnovaModel->setCustomModelMode(_customModel);

	ui->listModelTerms->setModel(model);


	ui->columnLabel0->setText(model->headerData(0, Qt::Horizontal, Qt::DisplayRole).toString());
	ui->columnLabel1->setText(model->headerData(1, Qt::Horizontal, Qt::DisplayRole).toString());

	variablesAvailableChanged();
	connect(_listModelAnovaModel, SIGNAL(variablesAvailableChanged()), this, SLOT(variablesAvailableChanged()));
}

void AnovaModelWidget::setCustomModelMode(bool customModel)
{
	_customModel = customModel;
	if (_listModelAnovaModel != NULL)
		_listModelAnovaModel->setCustomModelMode(customModel);
	ui->modelBuilder->setEnabled(customModel);
}

void AnovaModelWidget::variablesAvailableChanged()
{
	const QList<ColumnInfo> &variables = _listModelAnovaModel->variables();
	_listModelVariablesAvailable->setVariables(variables);
}

void AnovaModelWidget::sourceSelectionChanged()
{
	int selectedCount = ui->listFactorsAvailable->selectionModel()->selectedIndexes().size();

	_assignInteraction->setEnabled(selectedCount >= 2);
	_assignMainEffects->setEnabled(selectedCount >= 1);
	_assign2ways->setEnabled(selectedCount >= 2);
	_assign3ways->setEnabled(selectedCount >= 3);
	_assign4ways->setEnabled(selectedCount >= 4);
	_assign5ways->setEnabled(selectedCount >= 5);
}

void AnovaModelWidget::assignInteraction()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, ListModelAnovaModel::Interaction);
}

void AnovaModelWidget::assignMainEffects()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, ListModelAnovaModel::MainEffects);
}

void AnovaModelWidget::assign2ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, ListModelAnovaModel::All2Way);
}

void AnovaModelWidget::assign3ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, ListModelAnovaModel::All3Way);
}

void AnovaModelWidget::assign4ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, ListModelAnovaModel::All4Way);
}

void AnovaModelWidget::assign5ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, ListModelAnovaModel::All5Way);
}
