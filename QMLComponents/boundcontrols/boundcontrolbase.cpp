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


//mds-plan C8/D3.4: the send-time `.meta` of this option - THE single source since the mirror and
//its createMeta provenance-stamping era were deleted. It holds everything the engine consumes
//(shouldEncode gates the encode; isRCode/filterForValues drive the R-code and filter-by-name
//paths; families add encodeThis/loadFilteredData on top of this via the same virtual) and NEVER a
//dataSetId/filterId: provenance is in the encoded name (+ the transient `.filterIds` sidecar,
//createFilterIds below, which the slice-queue build and the request encoding consume and the
//engine strips before R - C8/C11). The host resolution that used to feed the mirror's stamp keys
//(generatedHostFilter) now serves the sidecar only.
Json::Value BoundControlBase::createEngineMeta()  const
{
	Json::Value meta(Json::objectValue);

	if (_control->encodeValue())
		meta["shouldEncode"] = true;

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

Filter *BoundControlBase::generatedHostFilter() const
{
	AnalysisBase * analysis = _control->form() ? _control->form()->analysisObj() : nullptr;

	if (!analysis || !analysis->multiDataSetAware())
		return nullptr;

	//mds-plan D4.2: LOCAL resolution (JASPControl::resolvedVarInfo - own host via the relation
	//chain, else the analysis-level VariableInfo) replaced the deleted walk; the provider of
	//whatever VariableInfo serves the control IS the provenance this option belongs to: the
	//selected Filter of an explicit host, or the analysis'/shown filter at analysis level
	//(C13 host-bound fallback - dynamic_cast, not selectedSelectionFilter(): the analysis-level
	//provider is a Filter too and the sidecar must name it as well). Re-read per call, never
	//cached (D1.2/D2.3).
	VariableInfo * varInfo = _control->resolvedVarInfo();
	Filter * stampFilter = varInfo ? dynamic_cast<Filter *>(varInfo->provider()) : nullptr;
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
	writeCurrentValue(value, emitChange);
}

//mds-plan D3.4 (the deletion): every value the control commits goes through HERE. The value lands
//in the component-owned `_boundValue` (the generators' home - C11) and the change propagates as
//pure notification: AnalysisBase::notifyOptionsChanged() re-emits boundValuesChanged + the aware
//dataSpecChanged (the tail the mirror's setBoundValue used to carry, gated exactly as before),
//and boundValueChanged drives control -> form -> analysis (the run trigger). Nothing is pushed
//into an option tree any more; bindTo, resetBoundValue and the per-family write paths all reach
//this through setBoundValue.
void BoundControlBase::writeCurrentValue(const Json::Value &value, bool emitChange)
{
	_boundValue = value;

	if (AnalysisForm * form = _control->form())
		if (AnalysisBase * analysis = form->analysisObj())
			analysis->notifyOptionsChanged();

	if (emitChange)
		emit _control->boundValueChanged(_control);
}

void BoundControlBase::setIsRCode(std::string key)
{
	_isRCode.insert(key);
}

void BoundControlBase::setFilterForValues(const std::string &filterName)
{
	_filterForValues = filterName;
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

