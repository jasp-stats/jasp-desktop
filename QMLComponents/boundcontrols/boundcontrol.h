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
//

#ifndef BOUNDCONTROL_H
#define BOUNDCONTROL_H

#include <json/json.h>

class BoundControl
{

public:
	//mds-plan C11/D3.1: the two JSON shapes generated ON DEMAND from the components' typed state
	//(the components are the source of truth; these generations are what replaces reading a
	//stored options tree):
	//  Engine  - the current R contract: exactly the option values the control writes today, plus
	//            the send-time `.meta` (createEngineMeta, C8: never dataSetId/filterId) and the
	//            transient per-term/per-option `.filterIds` sidecar (createFilterIds) that the
	//            slice-queue build and the request encoding consume and that is STRIPPED at the
	//            engine boundary before R ever sees it (C11, mds-plan D3.3).
	//  Storage - clean typed state (D5.1 preview; the final storage tweaks happen in Phase 5):
	//            no `.meta`, no sidecar tree - where the shape has no place for per-term
	//            filterIds they ride inside the term option itself as a `filterIds` member.
	//The QML-facing API maps these as integers 0/1 (AnalysisForm::createOptionsJsonAsJson).
	enum JsonFormat { Engine, Storage };

	virtual Json::Value					createJson()													const	= 0;
	virtual Json::Value					createMeta()													const	= 0;
	virtual bool						isJsonValid(const Json::Value& optionValue)						const	= 0;
	virtual void						bindTo(const Json::Value& value)										= 0;
	virtual const Json::Value&			boundValue()													const	= 0;
	virtual void						resetBoundValue()														= 0;
	virtual void						setBoundValue(const Json::Value& value, bool emitChange = true)			= 0;
	virtual const Json::Value&			defaultBoundValue()												const	= 0;
	virtual	void						setDefaultBoundValue(const Json::Value& defaultValue)					= 0;

	///< Generate this control's option value in the requested shape (mds-plan D3.1). The default
	/// is today's createJson() for both formats: BoundControlBase routes the two formats through
	/// createEngineValue()/createStorageValue() and the families whose stored value is built from
	/// live state (not from createJson's default builder) override those hooks, so generator and
	/// write path stay one implementation. Composite controls (TextAreaBase, VariablesListBase,
	/// TableViewBase) forward boundControl() to their companion, which answers here.
	virtual Json::Value	createJson(JsonFormat)														const	{ return createJson();			}
	///< The send-time `.meta` node of this option (mds-plan C8/D3.2): what createMeta() produces
	/// MINUS the dataSetId/filterId provenance keys. Default: no meta.
	virtual Json::Value	createEngineMeta()															const	{ return Json::nullValue;		}
	///< The transient per-term/per-option filterId node for this option (mds-plan C11/D3.2),
	/// placed at the option's path inside the generated `.filterIds` tree (same placement rules as
	/// `.meta`). Schema: int | array of (int|null); an array is POSITIONAL over the option's term
	/// values (null = unqualified term ⟹ the D3.7 resolver's fallback), a scalar covers the whole
	/// option (today's option-level stamp). Null when there is nothing to say. Stripped at the
	/// engine boundary (D3.3); never reaches Storage.
	virtual Json::Value	createFilterIds()															const	{ return Json::nullValue;		}

	///< Rewrite the stored option with a freshly created .meta (same value, no control-level
	/// change signal): used by a selecting host to move provenance stamps to a new filter
	/// without touching values (HostFilterSelection::restampValuesUnderHost). Base: nothing
	/// to restamp; BoundControlBase implements it for the controls that stamp at all.
	virtual void						restampValue()															{}
};

#endif // BOUNDCONTROL_H
