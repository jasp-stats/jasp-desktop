//
// Copyright (C) 2013-2020 University of Amsterdam
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

#include "boundcontrolbase.h"
#include "controls/jaspcontrol.h"
#include "controls/jasplistcontrol.h"
#include "analysisform.h"
#include "analysisbase.h"
#include "filter.h"
#include "dataset.h"
#include "log.h"
#include "models/listmodel.h"
#include "controls/rowcontrols.h"

BoundControlBase::BoundControlBase(JASPControl* control) : _control(control)
{
}


//To do: define these fields (isRCode, shouldEncode, dataSetId, etc) somewhere centrally through an enum or something
Json::Value BoundControlBase::createMeta()  const
{ 
	Json::Value meta(Json::objectValue);

	if (_control->encodeValue())
	{
		meta["shouldEncode"] = true;

		//Provenance for the values that actually ENCODE COLUMNS of a multi-dataset aware analysis:
		//which dataset (and which filter of it) the value was selected THROUGH - the owning host's
		//filter under a selecting VariablesForm/TextArea, else the analysis' own. The ids come
		//straight from the storage db, so they are still the right ones when the file reopens;
		//filter ids are globally unique and imply their dataset, so the engine queues its slices
		//from the stamped filter ids alone (ColumnEncoder::collectFilterIdsFromMeta, consumed by
		//DataBridge::prepareMultiDataSetRun) and attributes every encoded value to its own
		//dataset's encoder. Scalars carry no stamp on purpose (mds-plan C3/M5): they never rewrite
		//themselves when a host switches selection, so their stamp could only go stale - and a
		//stale stamped id seeds a phantom slice for a filter nothing uses anymore. Column-bearing
		//values stay live because a changing host restamps them (HostFilterSelection) and a
		//changing selection re-encodes them through the provider chain.
		//
		//mds-plan D3.1: the stamp resolution itself lives in generatedHostFilter(), which the
		//send-time generator's .meta (createEngineMeta = createMeta minus these two keys, C8) and
		//the transient `.filterIds` sidecar (createFilterIds) both read - mirror and generator
		//share one implementation, so they cannot drift before the consumer flip (D3.3/D3.5).
		if (Filter * stampFilter = generatedHostFilter())
		{
			meta["dataSetId"]	= stampFilter->data()->id();
			meta["filterId"]	= stampFilter->id();
		}
	}
	
	for(const std::string & key : _isRCode)
		if(key.empty())		meta	 ["isRCode"] = true;
		else				meta[key]["isRCode"] = true;
	
	if(!_filterForValues.empty()) //Then the control should load a filter by this name and give it as "values" in the options it passes to the analysis
		meta["filterForValues"] = _filterForValues;

	return meta;
}

Json::Value BoundControlBase::createJson(JsonFormat format) const
{
	return format == Engine ? createEngineValue() : createStorageValue();
}

Json::Value BoundControlBase::createEngineMeta() const
{
	//mds-plan C8/D3.1: the send-time .meta of this option is EXACTLY what createMeta() stamps for
	//the mirror, minus the two provenance keys (provenance rides the `.filterIds` sidecar and the
	//terms themselves from here on). Calling the virtual createMeta() keeps every family's meta
	//override (encodeThis, loadFilteredData, ...) in the generated tree without a second
	//implementation to drift.
	Json::Value meta = createMeta();
	meta.removeMember("dataSetId");
	meta.removeMember("filterId");
	return meta;
}

Filter *BoundControlBase::generatedHostFilter() const
{
	AnalysisBase * analysis = _control->form() ? _control->form()->analysisObj() : nullptr;

	if (!analysis || !analysis->multiDataSetAware())
		return nullptr;

	Filter * stampFilter = _control->effectiveSelectionFilter();
	return stampFilter && stampFilter->data() ? stampFilter : nullptr;
}

int BoundControlBase::generatedHostFilterId() const
{
	Filter * host = generatedHostFilter();
	return host ? host->id() : -1;
}

Json::Value BoundControlBase::createFilterIds() const
{
	//Default transient provenance node (mds-plan D3.2): the option-level host filter - exactly the
	//id createMeta stamps as filterId today, at exactly the same gate (encodeValue + aware analysis
	//+ resolvable host). Term-bearing families override this with the positional per-term array
	//(BoundControlTerms::createFilterIds); values that were never stamped (non-aware, released
	//files, hostless legacy runs) generate NO node at all, matching today's stampless meta.
	if (!_control->encodeValue())
		return Json::nullValue;

	const int hostId = generatedHostFilterId();
	return hostId >= 0 ? Json::Value(hostId) : Json::nullValue;
}


void BoundControlBase::handleComputedColumn(const Json::Value& value)
{
	if (_isColumn && value.isString())
	{
		const Json::Value & orgValue = boundValue();
		std::string			newName  = value.asString(),
							orgName  = orgValue.asString();

		if (newName != orgName)
		{
			if (!orgName.empty())
				emit _control->requestComputedColumnDestruction(orgName);
			
			if(!newName.empty())
			{
				if (_isComputedColumn)	emit _control->requestComputedColumnCreation(newName);
				else					emit _control->requestColumnCreation(newName, _columnType);
			}
			
		}
	}
}

void BoundControlBase::setBoundValue(const Json::Value &value, bool emitChange)
{
	AnalysisForm* form = _control->form();

	if (!form || !_control->isBound() || value == boundValue()) return;

	handleComputedColumn(value);
	writeCurrentValue(value, createMeta(), emitChange);
}

//mds-plan D3.4 (staged, see the header): every value the control commits goes through HERE -
//component-owned state first, the _boundValues mirror (while it exists) second, the change
//notification last. bindTo, resetBoundValue and the per-family write paths all funnel through
//setBoundValue, so this is the one place Phase 3 flips from "push into the tree" to pure
//control -> form -> analysis change signalling.
void BoundControlBase::writeCurrentValue(const Json::Value &value, const Json::Value &meta, bool emitChange)
{
	_boundValue = value;

	if (AnalysisForm * form = _control->form())
		form->setBoundValue(getName(), value, meta, _control->getParentKeys());

	if (emitChange)
		emit _control->boundValueChanged(_control);
}

void BoundControlBase::restampValue()
{
	AnalysisForm* form = _control->form();

	//Same option value, freshly created .meta, and no boundValueChanged: a selecting host uses
	//this to move the provenance of its (unchanged) column values onto a new filter. A control
	//whose option isn't in the json yet has nothing to restamp, and writing one here would
	//create an option that was never set.
	if (!form || !_control->isBound())
		return;

	const Json::Value &value = boundValue();

	if (value.isNull())
		return;

	form->setBoundValue(getName(), value, createMeta(), _control->getParentKeys());
}

void BoundControlBase::setIsRCode(std::string key)
{
	_isRCode.insert(key);
}

void BoundControlBase::setFilterForValues(const std::string &filterName)
{
	_filterForValues = filterName;
}

const Json::Value & BoundControlBase::boundValue() const
{
	AnalysisForm * form = _control->form();

	if (form)	return form->boundValue(getName(), _control->getParentKeys());
	else		return Json::Value::null;
}

void BoundControlBase::setIsColumn(bool isComputed, columnType type)
{
	_isColumn			= true;
	_isComputedColumn	= isComputed;
	_columnType			= type;

	AnalysisForm * form = _control->form();
	
	if (form)	form->addColumnControl(_control, isComputed);
}


std::string BoundControlBase::getName() const
{
	return fq(_control->name());
}

bool BoundControlBase::_isValueWithTypes(const Json::Value &value) const
{
	return value.isObject() && value.isMember("types") && value.isMember("value");
}

