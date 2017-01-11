//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "tablemodelanovamodel.h"

#include <QMimeData>
#include <QDebug>

#include "qutils.h"
#include <boost/foreach.hpp>

#include "options/optionboolean.h"

using namespace std;

TableModelAnovaModel::TableModelAnovaModel(QObject *parent)
	: TableModel(parent)
{
	_boundTo = NULL;

	_terms.setSortParent(_variables);
}

QVariant TableModelAnovaModel::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL || index.isValid() == false)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		int colNo = index.column();
		int rowNo = index.row();
		Options *row = _rows.at(rowNo);

		if (colNo == 0) {

			OptionTerms *termOption = static_cast<OptionTerms *>(row->get(0));
			Term t(termOption->value().front());
			return t.asQString();
		}
	}
	else if (role == Qt::CheckStateRole)
	{
		int colNo = index.column();
		int rowNo = index.row();
		Options *row = _rows.at(rowNo);

		if (colNo == 1)
		{
			OptionBoolean *booleanOption = static_cast<OptionBoolean *>(row->get(1));
			return booleanOption->value() ? Qt::Checked : Qt::Unchecked;
		}
	}

	return QVariant();
}

int TableModelAnovaModel::rowCount(const QModelIndex &) const
{
	if (_boundTo == NULL)
		return 0;

	return _rows.size();
}

int TableModelAnovaModel::columnCount(const QModelIndex &) const
{
	if (_boundTo == NULL)
		return 0;

	return _boundTo->rowTemplate()->size();
}

bool TableModelAnovaModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (index.isValid() == false)
		return false;

	if (index.column() == 1 && role == Qt::CheckStateRole)
	{
		bool checked = value.toInt() == Qt::Checked;

		Options *row = _rows.at(index.row());
		OptionBoolean *booleanOption = static_cast<OptionBoolean *>(row->get(1));
		booleanOption->setValue(checked);
		updateNuisances(checked);

		_boundTo->setValue(_rows);

		return true;
	}

	return false;
}

QStringList TableModelAnovaModel::mimeTypes() const
{
	QStringList types;

	types << "application/vnd.list.term";

	return types;
}

void TableModelAnovaModel::setVariables(const Terms &fixedFactors, const Terms &randomFactors, const Terms &covariates)
{
	_fixedFactors = fixedFactors;
	_randomFactors = randomFactors;
	_covariates = covariates;

	Terms all;
	all.add(fixedFactors);
	all.add(randomFactors);
	all.add(covariates);

	_variables.set(all);

	emit variablesAvailableChanged();
}

const Terms &TableModelAnovaModel::variables() const
{
	return _variables;
}

void TableModelAnovaModel::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	beginResetModel();

	_rows = _boundTo->value();

	foreach (Options *row, _rows)
	{
		OptionTerm *nameOption = static_cast<OptionTerm*>(row->get(0));
		vector<string> term = nameOption->term();

		_terms.add(Term(term));
	}

	endResetModel();
}

void TableModelAnovaModel::unbind()
{
	beginResetModel();

	_boundTo = NULL;
	_rows.clear();

	endResetModel();
}

void TableModelAnovaModel::mimeDataMoved(const QModelIndexList &indexes)
{
	// sort indices, and delete from end to beginning

	QModelIndexList sorted = indexes;
	qSort(sorted.begin(), sorted.end(), qGreater<QModelIndex>());

	int lastRowDeleted = -1;

	Terms terms = _terms;
	Terms toRemove;

	foreach (const QModelIndex &index, sorted)
	{
		int rowNo = index.row();

		if (rowNo != lastRowDeleted)
		{
			toRemove.add(terms.at(index.row()));
			terms.remove(index.row());
		}

		lastRowDeleted = rowNo;
	}

	foreach (const Term &rem, toRemove)
	{
		size_t i = 0;

		while (i < terms.size())
		{
			const Term &term = terms.at(i);

			if (term.containsAll(rem))
				terms.remove(i);
			else
				i++;
		}
	}

	setTerms(terms);
}

const Terms &TableModelAnovaModel::terms() const
{
	return _terms;
}

void TableModelAnovaModel::addFixedFactors(const Terms &terms)
{
	_fixedFactors.add(terms);
	_variables.add(terms);

	Terms existingTerms = _terms;

	Terms newTerms = _terms;
	newTerms.discardWhatDoesContainTheseComponents(_randomFactors);
	newTerms.discardWhatDoesContainTheseComponents(_covariates);
	existingTerms.add(newTerms.ffCombinations(terms));

	setTerms(existingTerms);

	emit variablesAvailableChanged();
}

void TableModelAnovaModel::addRandomFactors(const Terms &terms)
{
	_randomFactors.add(terms);
	_variables.add(terms);

	Terms newTerms = _terms;
	newTerms.add(terms);

	setTerms(newTerms, true);

	emit variablesAvailableChanged();
}

void TableModelAnovaModel::addCovariates(const Terms &terms)
{
	_covariates.add(terms);
	_variables.add(terms);

	Terms newTerms = _terms;
	newTerms.add(terms);

	setTerms(newTerms);

	emit variablesAvailableChanged();
}

void TableModelAnovaModel::removeVariables(const Terms &terms)
{
	_variables.remove(terms);
	_fixedFactors.remove(terms);
	_randomFactors.remove(terms);
	_covariates.remove(terms);

	Terms newTerms = _terms;
	newTerms.discardWhatDoesContainTheseComponents(terms);

	setTerms(newTerms);

	emit variablesAvailableChanged();
}

bool TableModelAnovaModel::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	Q_UNUSED(action);
	Q_UNUSED(row);
	Q_UNUSED(column);
	Q_UNUSED(parent);

	if (mimeTypes().contains("application/vnd.list.term"))
	{
		QByteArray encodedData = data->data("application/vnd.list.term");
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		if (stream.atEnd())
			return false;

		return true;
	}

	return false;
}

bool TableModelAnovaModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int assignType)
{
	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if ( ! data->hasFormat("application/vnd.list.term"))
		return false;


	QByteArray encodedData = data->data("application/vnd.list.term");

	Terms dropped;
	dropped.setSortParent(_variables);
	dropped.set(encodedData);

	Terms newTerms;

	switch (assignType)
	{
	case Cross:
		newTerms = dropped.crossCombinations();
		break;
	case Interaction:
		newTerms = dropped.wayCombinations(dropped.size());
		break;
	case MainEffects:
		newTerms = dropped.wayCombinations(1);
		break;
	case All2Way:
		newTerms = dropped.wayCombinations(2);
		break;
	case All3Way:
		newTerms = dropped.wayCombinations(3);
		break;
	case All4Way:
		newTerms = dropped.wayCombinations(4);
		break;
	case All5Way:
		newTerms = dropped.wayCombinations(5);
		break;
	default:
		(void)newTerms;
	}

	assign(newTerms);

	return true;
}

bool TableModelAnovaModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	return dropMimeData(data, action, row, column, parent, Cross);
}

QMimeData *TableModelAnovaModel::mimeData(const QModelIndexList &indexes) const
{
	/* returns dummy data. when the user drags entries away from this listbox
	 * it means that they're deleting entries, so there's no point populating
	 * this data object properly
	 */

	Q_UNUSED(indexes);

	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << 0;

	mimeData->setData("application/vnd.list.term", encodedData);

	return mimeData;
}

Qt::ItemFlags TableModelAnovaModel::flags(const QModelIndex &index) const
{
	if (index.isValid())
	{
		if (index.column() == 0)
			return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsDragEnabled;
		else
			return Qt::ItemIsEnabled | Qt::ItemIsUserCheckable;
	}
	else
	{
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
	}
}

QVariant TableModelAnovaModel::headerData(int section, Qt::Orientation orientation, int role) const
{	
	if (orientation == Qt::Horizontal)
	{
		if (role == Qt::DisplayRole)
		{
			if (section == 0)
				return "Model Terms";
			else if (section == 1)
				return "Is Nuisance";
		}
		else
		{
			if (section == 1 && role == Qt::SizeHintRole)
				return QSize(50, -1);
		}
	}

	return QVariant();
}

Qt::DropActions TableModelAnovaModel::supportedDropActions() const
{
	return Qt::CopyAction;
}

Qt::DropActions TableModelAnovaModel::supportedDragActions() const
{
	return Qt::MoveAction;
}

OptionTerm *TableModelAnovaModel::termOptionFromRow(Options *row)
{
	return static_cast<OptionTerm *>(row->get(0));
}

void TableModelAnovaModel::setTerms(const Terms &terms, bool newTermsAreNuisance)
{
	_terms.set(terms);

	if (_boundTo == NULL)
		return;

	beginResetModel();

	Terms::const_iterator itr;
	vector<Options *>::iterator otr;

	otr = _rows.begin();

	while (otr != _rows.end())
	{
		Options *row = *otr;
		OptionTerm *termCell = termOptionFromRow(row);
		Term existingTerm = Term(termCell->term());

		bool shouldRemove = true;

		itr = terms.begin();

		while (itr != terms.end())
		{
			const Term &term = *itr;

			if (term == existingTerm)
			{
				shouldRemove = false;
				break;
			}

			itr++;
		}

		if (shouldRemove)
		{
			_rows.erase(otr);
			delete row;
		}
		else
		{
			otr++;
		}
	}

	itr = terms.begin();

	for (size_t i = 0; i < terms.size(); i++)
	{
		const Term &term = *itr;

		if (i < _rows.size())
		{
			otr = _rows.begin();
			otr += i;
			Options *row = *otr;
			OptionTerm *termCell = termOptionFromRow(row);
			Term existingTerm = Term(termCell->term());

			if (existingTerm != term)
			{
				Options *row = static_cast<Options *>(_boundTo->rowTemplate()->clone());
				OptionTerms *termCell = static_cast<OptionTerms *>(row->get(0));
				termCell->setValue(term.scomponents());

				if (row->size() > 1 && newTermsAreNuisance)
				{
					OptionBoolean *nuisance = static_cast<OptionBoolean *>(row->get(1));
					nuisance->setValue(true);
				}

				_rows.insert(otr, row);
			}
		}
		else
		{
			Options *row = static_cast<Options *>(_boundTo->rowTemplate()->clone());
			OptionTerms *termCell = static_cast<OptionTerms *>(row->get(0));
			termCell->setValue(term.scomponents());

			if (row->size() > 1 && newTermsAreNuisance)
			{
				OptionBoolean *nuisance = static_cast<OptionBoolean *>(row->get(1));
				nuisance->setValue(true);
			}

			_rows.push_back(row);
		}

		itr++;
	}

	updateNuisances();

	_boundTo->setValue(_rows);

	endResetModel();

	emit termsChanged();
}

void TableModelAnovaModel::clear()
{
	setTerms(Terms());
}

void TableModelAnovaModel::assign(const Terms &terms)
{
	Terms t = _terms;
	t.add(terms);
	setTerms(t);
}

void TableModelAnovaModel::updateNuisances(bool checked)
{
	if (_rows.size() > 0)
	{
		Options *row = _rows.front();
		if (row->size() < 2)
			return; // no nuisance terms
	}

	// if a higher order interaction is specified as nuisance, then all lower order terms should be changed to nuisance as well

	for (size_t i = 0; i < _rows.size(); i++)
	{
		Options *row = _rows.at(i);
		OptionTerm *termOption = static_cast<OptionTerm*>(row->get(0));
		OptionBoolean *nuisanceOption = static_cast<OptionBoolean*>(row->get(1));
		Term term = Term(termOption->term());

		if (nuisanceOption->value() == checked)
		{
			for (size_t j = 0; j < _rows.size(); j++)
			{
				if (i == j)
					continue;

				Options *r = _rows.at(j);

				OptionTerm *tOption = static_cast<OptionTerm*>(r->get(0));
				OptionBoolean *nOption = static_cast<OptionBoolean*>(r->get(1));
				Term t = Term(tOption->term());

				if (checked)
				{
					if (term.containsAll(t))
						nOption->setValue(true);
				}
				else
				{
					if (t.containsAll(term))
						nOption->setValue(false);
				}
			}
		}
	}

	emit dataChanged(this->index(0,1), this->index(_rows.size() - 1, 1));
}
bool TableModelAnovaModel::piecesCanBeAssigned() const
{
	return _piecesCanBeAssigned;
}

void TableModelAnovaModel::setPiecesCanBeAssigned(bool piecesCanBeAssigned)
{
	_piecesCanBeAssigned = piecesCanBeAssigned;
}

