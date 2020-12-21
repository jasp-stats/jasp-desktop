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

#include "listmodellayersassigned.h"
#include "boundcontrollayers.h"

#include "utilities/qutils.h"

using namespace std;


ListModelLayersAssigned::ListModelLayersAssigned(JASPListControl* listView)
	: ListModelAssignedInterface(listView)
{
}

void ListModelLayersAssigned::initLayers(const std::vector<std::vector<std::string> >& allVariables)
{
	beginResetModel();
	
	for (const std::vector<std::string>& variables : allVariables)
	{
		QList<QString> layer;
		for (const std::string& variable : variables)
			layer.push_back(QString::fromStdString(variable));
		_variables.push_back(layer);
	}

	_setTerms();

	if (availableModel() != nullptr)
	{
		if (!_copyTermsWhenDropped)
			availableModel()->removeTermsInAssignedList();
	}
	
	endResetModel();
}

std::vector<std::pair<string, std::vector<string> > > ListModelLayersAssigned::getLayers() const
{
	std::vector<std::pair<string, std::vector<string> > > layers;
	
	int layerNr = 0;
	for (const QList<QString>& variables : _variables)
	{
		std::vector<string> layer;
		for (const QString& variable : variables)
			layer.push_back(variable.toStdString());
		layers.push_back(make_pair(tr("Layer %1").arg(layerNr).toStdString(), layer));
	}
	
	return layers;
}

int ListModelLayersAssigned::_getLayer(int index, int& indexInLayer, bool inclusive) const
{
	int layer = 0;
	int layerIndex = 0; // Layer 1
	indexInLayer = -1;
	
	if (inclusive) index--;
	while ((layer < _variables.length()) && (layerIndex + _variables[layer].length() < index))
	{
		layerIndex += _variables[layer].length() + 1;
		layer++;
	}
	
	if (inclusive) index++;
	if (layer < _variables.length())
		indexInLayer = index - layerIndex - 1;
		
	return layer;
}

void ListModelLayersAssigned::_setTerms()
{
	Terms newTerms;
	int layer = 1;
	for (const QList<QString>& variables : _variables)
	{
		newTerms.add(tr("Layer %1").arg(layer));
		for (const QString& variable : variables)
			newTerms.add(variable);
		layer++;
	}

	newTerms.add(tr("Layer %1").arg(layer));

	ListModel::_setTerms(newTerms);
}

Terms ListModelLayersAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms terms;

	for (int index : indexes)
	{
		int indexInLayer = -1;
		int layer = _getLayer(index, indexInLayer);
		if (layer < _variables.length())
		{
			if (indexInLayer >= 0 && indexInLayer < _variables[layer].length())
				terms.add(Term(_variables[layer][indexInLayer]));
		}
	}
	
	return terms;
}

Terms ListModelLayersAssigned::addTerms(const Terms& terms, int dropItemIndex, JASPControl::AssignType)
{
	Terms result;
	beginResetModel();
	
	int layer = _variables.length();
	int indexInLayer = 0;
	if (dropItemIndex >= 0)
		layer = _getLayer(dropItemIndex, indexInLayer, true);
	
	if (layer >= _variables.length())	
	{
		_variables.push_back(QList<QString>());
		layer = _variables.length() - 1;
		indexInLayer = 0;
	}
	
	if (indexInLayer < 0)
		indexInLayer = 0;
	
	for (const Term& term : terms)
		_variables[layer].insert(indexInLayer, term.asQString());

	_setTerms();
	
	endResetModel();
	
	return result;
}

void ListModelLayersAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{	
	beginResetModel();
	
	int layerDrop = _variables.length();
	int indexInLayerDrop = 0;
	if (dropItemIndex >= 0)
		layerDrop = _getLayer(dropItemIndex, indexInLayerDrop, true);
	
	if (layerDrop >= _variables.length())	
	{
		_variables.push_back(QList<QString>());
		layerDrop = _variables.length() - 1;
		indexInLayerDrop = 0;
	}
	
	if (indexInLayerDrop < 0)
		indexInLayerDrop = 0;
	
	QList<QString> movedVariables;
	QList<int> sortedIndexes = indexes;
	std::sort(sortedIndexes.begin(), sortedIndexes.end(), std::greater<int>());
	// Store first the variables that must be moved, before removing them in the _variables list:
	// removing the items in the _variables list will change the indexes
	for (int index : sortedIndexes)
	{
		int indexInLayer = 0;
		int layer = _getLayer(index, indexInLayer);
		
		if (layer < _variables.length() && indexInLayer >= 0 && indexInLayer < _variables[layer].length())
			movedVariables.push_back(_variables[layer][indexInLayer]);
	}
	
	for (int index : sortedIndexes)
	{
		int indexInLayer = 0;
		int layer = _getLayer(index, indexInLayer);
		
		if (layer < _variables.length() && indexInLayer >= 0 && indexInLayer < _variables[layer].length())
		{
			if (layer == layerDrop && indexInLayer < indexInLayerDrop)
				indexInLayerDrop--;
			_variables[layer].removeAt(indexInLayer);
		}
	}
	
	for (const QString& variable : movedVariables)
		_variables[layerDrop].insert(indexInLayerDrop, variable);
	
	for (int i = _variables.length() - 1; i >= 0; i--)
	{
		if (_variables[i].length() == 0)
			_variables.removeAt(i);
	}

	_setTerms();
	
	endResetModel();	
}

void ListModelLayersAssigned::removeTerms(const QList<int> &indexes)
{
	beginResetModel();
	
	QList<int> sortedIndexes = indexes;
	std::sort(sortedIndexes.begin(), sortedIndexes.end(), std::greater<int>());


	for (int index : sortedIndexes)
	{
		int layer = _variables.length();
		int indexInLayer = 0;

		layer = _getLayer(index, indexInLayer);
		if (layer >= 0 && layer < _variables.length() && indexInLayer >= 0 && indexInLayer < _variables[layer].length())
			_variables[layer].removeAt(indexInLayer);

	}
	
	for (int i = _variables.length() - 1; i >= 0; i--)
	{
		if (_variables[i].length() == 0)
			_variables.removeAt(i);
	}

	_setTerms();
	
	endResetModel();	
}

QVariant ListModelLayersAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
		return QVariant();

	QVariant result;
	
	int indexInLayer = -1;
	int layer = _getLayer(index.row(), indexInLayer);
	
	if (role == ListModel::SelectableRole)
	{
		result = (indexInLayer >= 0);
	}
	else if (role == ListModel::TypeRole)
	{
		if (indexInLayer < 0)
		{
			QString type = "layer";
			if (layer == _variables.length())
				type += ",virtual";
			result = type;
		}
		else
			result = "variable";
	}
	else if (role == ListModel::ColumnTypeRole)
	{
		if (layer >= 0 && layer < _variables.length() && indexInLayer >= 0 && indexInLayer < _variables[layer].length())
		{
			QString variable = _variables[layer][indexInLayer];
			result = requestInfo(variable, VariableInfo::VariableTypeName).toString();
		}
	}
	else
		result = ListModelAssignedInterface::data(index, role);


	return result;
}
