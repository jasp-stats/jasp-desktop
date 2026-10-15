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

#include "hostfilterselection.h"
#include "jaspcontrol.h"
#include "analysisform.h"
#include "analysisbase.h"
#include "boundcontrols/boundcontrol.h"
#include "workspace.h"
#include "filter.h"
#include "variableinfo.h"
#include "log.h"

HostFilterSelection::HostFilterSelection(JASPControl * host) : QObject(host), _host(host)
{
}

void HostFilterSelection::setOption(const QString & option)
{
	if (_option == option)
		return;

	const QString previous = _option;
	_option = option;

	//Registry membership follows the property (mds-plan C4/REGISTRY): the syntax bridge reads
	//the form's registry instead of scanning children, and a re-registered host applies its own
	//pending raw selection value (left behind by a parse that ran before this host existed)
	//immediately. When the form is not yet known here, analysisBecameKnown() registers later.
	//Hosts are per-LIST/TextArea selection owners (mds-plan C10/D2.2), see VariablesListBase.
	if (AnalysisForm * form = _host->form())
	{
		if (!previous.isEmpty())
			form->unregisterDataSetSelectionOption(previous, _host);
		if (!_option.isEmpty())
			form->registerDataSetSelectionOption(_option, _host);
	}

	emit selectionAvailableChanged();

	//The analysis may well be known already (property set after form setup):
	analysisBecameKnown();
}

void HostFilterSelection::setSelectedFilterId(int filterId)
{
	if (filterId == _selectedFilterId)
		return;

	Workspace * workspace = Workspace::singleton();
	Filter    * filter    = workspace ? workspace->filterById(filterId) : nullptr;

	//An id that names no filter carries no information: reject it (keep selection and option as
	//they were) rather than store a dangling number the engine could never slice on.
	if (!filter)
	{
		Log::log() << "HostFilterSelection: ignoring selection of unknown filter id " << filterId
		           << " (option '" << option() << "'), keeping " << _selectedFilterId << std::endl;
		return;
	}

	_explicitChoice = true;
	applyFilter(filter);
}

VariableInfo * HostFilterSelection::ownVarInfo()
{
	if (!_ownVarInfo)
	{
		_ownVarInfo = new VariableInfo(nullptr, _host);

		if (_selectedFilter)
			_ownVarInfo->setProvider(_selectedFilter);
		else if (Filter * fallback = currentFallbackFilter())
			_ownVarInfo->setProvider(fallback);
	}

	return _ownVarInfo;
}

QVariantList HostFilterSelection::filterSelectionValues() const
{
	Workspace * workspace = Workspace::singleton();
	return workspace ? workspace->dataSetFilterDropDownList() : QVariantList();
}

void HostFilterSelection::analysisBecameKnown()
{
	if (!_host->form() || _option.isEmpty())
		return;

	//The property may have been set before this host knew its form (async instantiation, the
	//M9 case): registration is idempotent, so doing it again here is harmless - and it is what
	//puts dynamically created selection hosts into the bridge's registry at all.
	_host->form()->registerDataSetSelectionOption(_option, _host);
	//A selection option makes this a multi-dataset analysis by definition: the desktop entry
	//pushes that flag from the module data, but the syntax bridge runs on a dummy analysis that
	//gets nothing else - so declare it here, before any option binds and .meta gets stamped.
	AnalysisBase * analysis = _host->form()->analysisObj();

	if (analysis && !analysis->multiDataSetAware())
		analysis->setMultiDataSetAware(true);

	if (Workspace * workspace = Workspace::singleton())
	{
		//C10: when the selected filter (or its whole dataset) vanishes the selection must not
		//dangle: fall back to another filter, or stay empty when nothing is left.
		connect(workspace, &Workspace::dataSetRemoved,		this, &HostFilterSelection::validateSelection, Qt::UniqueConnection);
		connect(workspace, &Workspace::filtersCountChanged,	this, &HostFilterSelection::validateSelection, Qt::UniqueConnection);
		connect(workspace, &Workspace::dataSetFilterDropDownListChanged, this, &HostFilterSelection::filterSelectionValuesChanged, Qt::UniqueConnection);
	}

	if (_selectedFilterId == -1)
		applyFilter(currentFallbackFilter());
}

void HostFilterSelection::analysisFilterMaybeChanged()
{
	if (!selectionAvailable() || _explicitChoice)
		return;

	applyFilter(currentFallbackFilter());
}

void HostFilterSelection::validateSelection()
{
	if (_selectedFilterId == -1)
		return;

	Workspace * workspace = Workspace::singleton();

	if (workspace && workspace->filterById(_selectedFilterId) == _selectedFilter)
		return;

	_explicitChoice = false;
	applyFilter(currentFallbackFilter());
}

void HostFilterSelection::applyFilter(Filter * filter)
{
	const int newId = filter ? filter->id() : -1;

	if (newId == _selectedFilterId && filter == _selectedFilter)
		return;

	_selectedFilter   = filter;
	_selectedFilterId = newId;

	if (_ownVarInfo)
		_ownVarInfo->setProvider(filter ? filter : currentFallbackFilter());

	//The provider is live from here on: move the provenance stamps of the column values served
	//by this selection onto the new filter BEFORE anybody downstream reads the options again.
	restampValuesUnderHost();

	emit selectedFilterIdChanged();
}

//The .meta stamps of column-bearing values name the filter they were selected THROUGH
//(BoundControlBase::createMeta), so a new selection makes the ones below this host stale. A
//same-dataset switch rewrites no value at all - the encoded column names stay valid - so without
//this the abandoned filter would keep a stamp (and the engine a phantom slice, and
//referencedDataSets()/dataSpec()/RPC dataSetIds an over-reported dataset) while the live one had
//none. Values are not touched: each control's .meta is simply rewritten through the provider
//chain, which now resolves to `filter`. Scalars are walked too but createMeta no longer stamps
//them (mds-plan C3/M5), so this can never (re)introduce a stamp that shouldn't be there.
void HostFilterSelection::restampValuesUnderHost()
{
	AnalysisForm * form = _host->form();
	AnalysisBase * analysis = form ? form->analysisObj() : nullptr;

	if(!analysis || !analysis->multiDataSetAware())
		return;

	//Exactly the controls whose variables resolve through THIS host: a nested host serves its
	//own subtree, and the resolution follows `source` (and the related available list, mds-plan
	//C10) too, so a list fed from below this host - even one visually parked elsewhere - is
	//restamped where its names actually come from. The host itself is included (mds-plan D2.2):
	//a list host's own bound terms resolve through its own selection - for the hosts that were
	//never bound (VariablesForm) this made no difference, for TextArea it re-stamps the same
	//filter the value already had.
	const QList<JASPControl *> controls = form->findChildren<JASPControl *>();

	for(JASPControl * control : controls)
	{
		if(!control->isBound() || !control->encodeValue())
			continue;

		if(control->selectionHost() != _host)
			continue;

		if(BoundControl * bound = control->boundControl())
			bound->restampValue();
	}
}

Filter * HostFilterSelection::currentFallbackFilter() const
{
	AnalysisBase * analysis = _host->form() ? _host->form()->analysisObj() : nullptr;

	if (analysis && analysis->filter())
		return analysis->filter();

	Workspace * workspace = Workspace::singleton();
	return workspace ? workspace->shownFilter() : nullptr;
}
