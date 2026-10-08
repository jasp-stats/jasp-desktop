//
// Copyright (C) 2013-2026 University of Amsterdam
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
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef HOSTFILTERSELECTION_H
#define HOSTFILTERSELECTION_H

#include <QObject>
#include <QPointer>
#include <QString>
#include <QVariantList>
#include "filter.h"							//QPointer<Filter> needs the complete type (multiple inheritance!)

class JASPControl;
class VariableInfo;

/// Dataset/filter selection that lives on a HOST control (a VariablesFormBase, a TextAreaBase,
/// ...) instead of on the analysis: two hosts of one analysis can therefore hold two different
/// filters at the same time - which is the whole point of the multi-dataset aware analyses.
///
/// The selection is a filter id, carried by the option named by dataSetSelectionOption: a
/// FilterSelect DropDown embedded by the host mirrors (and writes) that option, so the selection
/// travels to the engine identically in desktop and syntax mode. Being a filter id, the selection
/// always implies the dataset too, and the Filter itself serves the data: the host owns a
/// VariableInfo whose provider is the selected Filter (Filter *is* a VariableInfoProvider,
/// scoped to its own dataset, rows filtered, names encoded by that dataset's own encoder), and
/// every variable-info consumer below the host resolves to that VariableInfo (see
/// JASPControl::effectiveVarInfo).
///
/// The analysis' own filter (FilterMenuButton) is a separate, analysis-global thing: selection
/// code never touches it.
class HostFilterSelection : public QObject
{
	Q_OBJECT

public:
	explicit HostFilterSelection(JASPControl * host);

	void				  setOption(const QString & option);
	const QString & 	  option() const									{ return _option; }
	bool				  selectionAvailable() const						{ return !_option.isEmpty(); }

	///< -1 while there is no (valid) selection. Unknown ids are rejected: selection stays as is.
	int					  selectedFilterId() const						{ return _selectedFilterId; }
	void				  setSelectedFilterId(int filterId);
	Filter				* selectedFilter() const						{ return _selectedFilter; }

	///< The host's own VariableInfo (created on first use); provider is the selected filter, or
	///< the analysis'/workspace's current filter as long as nothing was picked. Only handed to
	///< consumers when selectionAvailable().
	VariableInfo		* ownVarInfo();

	///< {value: "<filterId>", label: "DataSet - Filter"} of every filter of every dataset.
	QVariantList		  filterSelectionValues() const;

	///< Host hook: called once the analysis is known. A selection option makes the analysis
	///  multiDataSetAware (in the bridge nothing else sets the flag) and establishes the default
	///  selection: the analysis' current filter, followed until an explicit choice is made.
	void				  analysisBecameKnown();

	///< Host hook: the analysis' own filter changed elsewhere (FilterMenuButton, RPC, ...). While
	///  no explicit choice was made the host follows it; a made choice is never overwritten.
	void				  analysisFilterMaybeChanged();

signals:
	void selectedFilterIdChanged();
	void selectionAvailableChanged();
	void filterSelectionValuesChanged();

private slots:
	void validateSelection();							///< C10: vanished filter -> fall back, never dangle

private:
	void   applyFilter(Filter * filter);				///< store + restamp provider + notify
	Filter * currentFallbackFilter() const;				///< analysis filter, else workspace shown filter

	JASPControl *				_host;
	QString						_option;
	int							_selectedFilterId	= -1;
	QPointer<Filter>			_selectedFilter;
	VariableInfo		 *	_ownVarInfo			= nullptr;
	bool						_explicitChoice		= false;
};

#endif // HOSTFILTERSELECTION_H
