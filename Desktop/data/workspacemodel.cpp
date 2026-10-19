#include "workspacemodel.h"
#include "datasetpackage.h"
#include "qutils.h"
#include "gui/preferencesmodel.h"
#include "undostack.h"

WorkspaceModel* WorkspaceModel::_singleton = nullptr;

WorkspaceModel::WorkspaceModel(QObject *parent)
	: QObject(parent)
{
	if(_singleton) throw std::runtime_error("WorkspaceModel can be constructed only once!");

	_singleton = this;

	connect(DataSetPackage::pkg(),	&DataSetPackage::loadedChanged,					this,	&WorkspaceModel::refresh				);
	connect(DataSetPackage::pkg(),	&DataSetPackage::shownDataSetChanged,			this,	&WorkspaceModel::refresh				);
	connect(DataSetPackage::pkg(),	&DataSetPackage::nameChanged,					this,	&WorkspaceModel::nameChanged			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::descriptionChanged,			this,	&WorkspaceModel::descriptionChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::workspaceEmptyValuesChanged,	this,	&WorkspaceModel::emptyValuesChanged		);
}

void WorkspaceModel::refresh()
{
	emit nameChanged();
	emit descriptionChanged();
	emit emptyValuesChanged();
}

QStringList WorkspaceModel::emptyValues() const
{
	DataSet * set = DataSetPackage::pkg()->workspace() ? DataSetPackage::pkg()->workspace()->shownDataSet() : nullptr;
	return tql(set ? set->emptyValuesAsStrings() : stringset());
}

QString WorkspaceModel::name() const
{
	return DataSetPackage::pkg()->name();
}

QString WorkspaceModel::description() const
{
	return DataSetPackage::pkg()->description();
}

void WorkspaceModel::setDescription(const QString &desc)
{
	if (desc == description()) return;
	if(!DataSetPackage::pkg()->dataSet()) return;

	UndoStack::singleton()->pushCommand(new SetWorkspacePropertyCommand(DataSetPackage::pkg()->dataSet(), desc, SetWorkspacePropertyCommand::WorkspaceProperty::Description));
}

void WorkspaceModel::removeEmptyValue(const QString &value)
{
	if(!DataSetPackage::pkg()->dataSet()) return;
	QStringList values = tql(DataSetPackage::pkg()->dataSet()->emptyValuesAsStrings());

	if (values.removeAll(value) > 0)
		UndoStack::singleton()->pushCommand(new SetWorkspaceEmptyValuesCommand(DataSetPackage::pkg()->dataSet(), values));
}

void WorkspaceModel::addEmptyValue(const QString &value)
{
	if(!DataSetPackage::pkg()->dataSet()) return;
	QStringList values = tql(DataSetPackage::pkg()->dataSet()->emptyValuesAsStrings());

	if (!values.contains(value))
	{
		values.push_back(value);
		UndoStack::singleton()->pushCommand(new SetWorkspaceEmptyValuesCommand(DataSetPackage::pkg()->dataSet(), values));
	}
}

void WorkspaceModel::resetEmptyValues()
{
	if(!DataSetPackage::pkg()->dataSet() || !PreferencesModel::prefs()) return;
	QStringList defaultValues = PreferencesModel::prefs()->emptyValues();

	if (defaultValues != emptyValues())
		UndoStack::singleton()->pushCommand(new SetWorkspaceEmptyValuesCommand(DataSetPackage::pkg()->dataSet(), defaultValues));
}
