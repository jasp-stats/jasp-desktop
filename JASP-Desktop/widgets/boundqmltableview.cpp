//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "boundqmltableview.h"
#include "analysis/options/optionvariable.h"
#include "analysis/options/optionlist.h"
#include "analysis/options/optionboolean.h"

#include <QQmlProperty>


BoundQMLTableView::BoundQMLTableView(QQuickItem *item, AnalysisQMLForm *form) : BoundQMLItem(item, form)
{
	_tableModel = new ListModelTable(form, item);
	
	connect(item, SIGNAL(comboBoxActivated(int, QString)), this, SLOT(comboBoxActivatedHandler(int, QString)));
}

void BoundQMLTableView::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
}

void BoundQMLTableView::unbind()
{
	
}

Option *BoundQMLTableView::createOption()
{
	
	// TODO: make it more generic!!!
	Options* options = new Options();
	OptionVariable* optionVariable = new OptionVariable();
	options->add("variable", optionVariable);
	OptionList* optionList = new OptionList();
	optionList->setValue("none");
	options->add("contrast", optionList);
	return new OptionsTable(options);	
	
}

void BoundQMLTableView::setUp()
{
	BoundQMLItem::setUp();
	_tableModel->setUp();
	QStringList syncModels = QQmlProperty(_item, "syncModels").read().toStringList();
	if (syncModels.isEmpty())
	{
		addError(QString::fromLatin1("Needs sync model for TableView ") + _name);
	}
	else
	{
		for (const QString& syncModel : syncModels)
		{
			if (syncModel != "_JASPAllVariables")
			{
				ListModel* model = _form->getModel(syncModel);
				if (model)
				{
					_syncModels.push_back(model);
					connect(model, &ListModel::termsChanged, this, &BoundQMLTableView::syncTermsChanged);
				}
				else
					addError(QString::fromLatin1("Unknown sync model ") + syncModel + QString::fromLatin1(" for ModelVIew ") + _name);
			}
		}
		resetTermsFromSyncModels();
	}
	
}

void BoundQMLTableView::syncTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	Q_UNUSED(termsAdded);
	Q_UNUSED(termsRemoved);
	
	resetTermsFromSyncModels();
}

void BoundQMLTableView::comboBoxActivatedHandler(int row, QString value)
{
	qDebug() << "comboBox activated, row " << row << ", value " << value;
	
	std::vector<Options *> groups;
	for (int i = 0; i < _terms.size(); ++i) {
		Options *newRow = static_cast<Options *>(_boundTo->rowTemplate()->clone());
		
		// TODO: set the variables and the combo box
		groups.push_back(newRow);
	}

	_boundTo->setValue(groups);
	
}

void BoundQMLTableView::resetTermsFromSyncModels()
{
	Terms termsAvailable;
	for (ListModel* syncModel : _syncModels)
	{
		const Terms& terms = syncModel->terms();
		termsAvailable.add(terms);
	}
	
	_tableModel->setTerms(termsAvailable);
}
