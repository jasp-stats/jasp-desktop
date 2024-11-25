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
#include "controls/jasplistcontrol.h"

using namespace std;


ListModelLayersAssigned::ListModelLayersAssigned(JASPListControl* listView)
	: ListModelAssignedInterface(listView)
{
}

void ListModelLayersAssigned::initLayers(const std::vector<std::vector<std::string> >& allVariables)
{
	beginResetModel();
	
	_variablesPerLayer.clear();
	for (const std::vector<std::string>& variables : allVariables)
	{
		QList<QString> layer;
		for (const std::string& variable : variables)
			layer.push_back(QString::fromStdString(variable));
		_variablesPerLayer.push_back(layer);
	}

	_setTerms();

	if (availableModel())
		availableModel()->removeTermsInAssignedList();
	
	endResetModel();
}

std::vector<std::pair<string, std::vector<string> > > ListModelLayersAssigned::getLayers() const
{
	std::vector<std::pair<string, std::vector<string> > > layers;
	
	int layerNr = 0;
	for (const QList<QString>& variables : _variablesPerLayer)
	{
		layerNr++;
		std::vector<string> layer;
		for (const QString& variable : variables)
			layer.push_back(variable.toStdString());
		layers.push_back(make_pair(tr("Layer %1").arg(layerNr).toStdString(), layer));
	}
	
	return layers;
}

std::pair<int, int> ListModelLayersAssigned::_getLayer(int row, bool insertVariable) const
{
	int rowCounter = 0, // count the rows until it gets the right layer
		layer = 0,
		variableIndiceInLayer = -1;
	
	while ((layer < _variablesPerLayer.length()) && (rowCounter + _variablesPerLayer[layer].length() + (insertVariable ? 1 : 0) < row))
	{
		rowCounter += _variablesPerLayer[layer].length() + 1; // number of variables + layer header
		layer++;
	}

	if (layer < _variablesPerLayer.length())
		variableIndiceInLayer = row - rowCounter - 1;
		
	return make_pair(layer, variableIndiceInLayer);
}

void ListModelLayersAssigned::_setTerms()
{
	// Add only the variables in terms: as this is an assigned variables list, the terms must be in the available variables list
	// But the available variables list does not have the layer headers.
	Terms newTerms;
	for (const QList<QString>& variables : _variablesPerLayer)
		for (const QString& variable : variables)
			newTerms.add(Term(variable, listView()->defaultType()));

	ListModel::_setTerms(newTerms);
}

Terms ListModelLayersAssigned::addTerms(const Terms& terms, int dropItemIndex, const RowControlsValues&)
{
	Terms result;
	
	if(!terms.size())
		return result;
	
	beginResetModel();
	
	auto [layer, indexInLayer] = (dropItemIndex >= 0) ? _getLayer(dropItemIndex, true) : std::make_pair(int(_variablesPerLayer.length()), 0);
	
	if (layer >= _variablesPerLayer.length())
	{
		_variablesPerLayer.push_back(QList<QString>());
		layer = _variablesPerLayer.length() - 1;
		indexInLayer = 0;
	}
	
	if (indexInLayer < 0)
		indexInLayer = 0;
	
	for (const Term& term : terms)
		_variablesPerLayer[layer].insert(indexInLayer, term.asQString());

	_setTerms();
	
	endResetModel();
	
	return result;
}

void ListModelLayersAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{	
	beginResetModel();
	
	auto [layerDrop, indexInLayerDrop] = (dropItemIndex >= 0) ? _getLayer(dropItemIndex, true): std::make_pair(int(_variablesPerLayer.length()), 0);
	
	if (layerDrop >= _variablesPerLayer.length())
	{
		_variablesPerLayer.push_back(QList<QString>());
		layerDrop = _variablesPerLayer.length() - 1;
		indexInLayerDrop = 0;
	}
	
	if (indexInLayerDrop < 0)
		indexInLayerDrop = 0;
	
	QList<QString> movedVariables;
	QList<int> sortedIndexes = indexes;
	std::sort(sortedIndexes.begin(), sortedIndexes.end(), std::greater<int>());
	// Store first the variables that must be moved, before removing them in the variables list:
	// removing the items in the _variables list will change the indexes
	for (int index : sortedIndexes)
	{
		auto [layer, indexInLayer] = _getLayer(index);

		if (layer < _variablesPerLayer.length() && indexInLayer >= 0 && indexInLayer < _variablesPerLayer[layer].length())
			movedVariables.push_back(_variablesPerLayer[layer][indexInLayer]);
	}
	
	for (int index : sortedIndexes)
	{
		auto [layer, indexInLayer] = _getLayer(index);
		
		if (layer < _variablesPerLayer.length() && indexInLayer >= 0 && indexInLayer < _variablesPerLayer[layer].length())
		{
			if (layer == layerDrop && indexInLayer < indexInLayerDrop)
				indexInLayerDrop--;
			_variablesPerLayer[layer].removeAt(indexInLayer);
		}
	}
	
	for (const QString& variable : movedVariables)
		_variablesPerLayer[layerDrop].insert(indexInLayerDrop, variable);
	
	for (int i = _variablesPerLayer.length() - 1; i >= 0; i--)
	{
		if (_variablesPerLayer[i].length() == 0)
			_variablesPerLayer.removeAt(i);
	}

	_setTerms();
	
	endResetModel();
}

void ListModelLayersAssigned::removeTerms(const QList<int> &indexes)
{
	if(!indexes.count())
		return;
	
	beginResetModel();
	
	QList<int> sortedIndexes = indexes;
	std::sort(sortedIndexes.begin(), sortedIndexes.end(), std::greater<int>());


	for (int index : sortedIndexes)
	{
		auto [layer, indexInLayer] = _getLayer(index);

		if (layer >= 0 && layer < _variablesPerLayer.length() && indexInLayer >= 0 && indexInLayer < _variablesPerLayer[layer].length())
			_variablesPerLayer[layer].removeAt(indexInLayer);

	}
	
	for (int i = _variablesPerLayer.length() - 1; i >= 0; i--)
	{
		if (_variablesPerLayer[i].length() == 0)
			_variablesPerLayer.removeAt(i);
	}

	_setTerms();
	
	endResetModel();
}

int ListModelLayersAssigned::rowCount(const QModelIndex &parent) const
{
	// terms().size() : number of variables
	// _variablesPerLayer.count(): number of layers
	// + 1 extra virtual layer
	return terms().size() + _variablesPerLayer.count() + 1;
}

QVariant ListModelLayersAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
		return QVariant();

	auto [layer, indexInLayer] = _getLayer(index.row());
	
	if (role == ListModel::SelectableRole)
		return (indexInLayer >= 0);
	else if (role == ListModel::TypeRole)
	{
		if (indexInLayer < 0)
		{
			QString type = "layer";
			if (layer == _variablesPerLayer.length())
				type += ",virtual";
			return type;
		}
		else
			return "variable";
	}
	else if (indexInLayer == -1)
	{
		if (role == Qt::DisplayRole || role == ListModel::NameRole)
			return tr("Layer %1").arg(layer+1);
		else if (role == ListModel::ColumnTypeRole)
			return columnTypeToQString(columnType::unknown);
	}
	else
		return ListModelAssignedInterface::data(index, role);

	return QVariant();
}

QList<int> ListModelLayersAssigned::indexesFromTerms(const Terms &terms) const
{
	QList<int> indexes;

	for (const Term& term : terms)
	{
		bool found = false;
		int ind = 0;
		for (auto & variables : _variablesPerLayer)
		{
			ind++;
			for (const QString& var : variables)
			{
				if (var == term.asQString())
				{
					found = true;
					indexes.append(ind);
					break;
				}
				ind++;
			}
			if (found)
				break;

		}
	}

	return indexes;
}

Terms ListModelLayersAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms result;
	for (int i : indexes)
	{
		auto [layer, variableIndInLayer] = _getLayer(i);
		if (variableIndInLayer >= 0)
			result.add(_variablesPerLayer[layer][variableIndInLayer]);
	}

	return result;
}
