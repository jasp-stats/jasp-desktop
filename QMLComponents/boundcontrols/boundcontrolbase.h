//
// Copyright (C) 2013-2021 University of Amsterdam
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

#ifndef BOUNDCONTROLBASE_H
#define BOUNDCONTROLBASE_H

#include "boundcontrol.h"
#include "columntype.h"
#include "models/listmodel.h"

class JASPControl;
class Terms;
class Filter;

class BoundControlBase : public BoundControl
{
public:
	BoundControlBase(JASPControl* control);
	virtual						~BoundControlBase()	{}

	Json::Value					createJson()												const	override { return Json::nullValue;		}
	Json::Value					createJson(JsonFormat format)										const	override;
	Json::Value					createEngineMeta()												const	override;
	Json::Value					createFilterIds()												const	override;
	Json::Value					createMeta()												const	override;
	void						bindTo(const Json::Value& value)									override { _orgValue = value; setBoundValue(value, false); }
	const Json::Value&			boundValue()												const	override;
	void						resetBoundValue()													override { bindTo(_orgValue); }
	void						setBoundValue(const Json::Value& value, bool emitChange = true)		override;
	void						restampValue()														override;
	void						setIsRCode(std::string key = "");
	void						setFilterForValues(const std::string & filterName);
	void						setIsColumn(bool isComputed, columnType type = columnType::unknown);
	const Json::Value&			defaultBoundValue()											const	override { return _defaultValue; }
	void						setDefaultBoundValue(const Json::Value& defaultValue)				override { _defaultValue = defaultValue; }

protected:
	std::string					getName()													const;
	void						handleComputedColumn(const Json::Value& value);

	bool						_isValueWithTypes(const Json::Value &value)					const;

	///< Engine-shape value generated from this control's LIVE typed state (mds-plan D3.1): the
	/// option value as the control's own write path produces it. The default is createJson(),
	/// which is exactly the written value for the families that build their write from createJson;
	/// the families whose mirror value comes from a live-model builder (tableviews, components
	/// lists, factor levels, JAGS/rlang textareas, ...) override this AND route their write path
	/// through the same builder, so generator and mirror cannot drift while both exist.
	virtual Json::Value			createEngineValue()												const	{ return createJson();			}
	///< Storage-shape value (D5.1 preview): clean typed state. Defaults to the engine value -
	/// the shapes only differ where per-term filterIds must ride inside the option itself
	/// (term lists), see BoundControlTerms::createStorageValue.
	virtual Json::Value			createStorageValue()											const	{ return createEngineValue();	}
	///< The filter this control's values are provenanced through at generation time, or nullptr:
	/// multiDataSetAware analysis + a resolvable effectiveSelectionFilter holding its dataset.
	/// SINGLE source of truth for the host stamp: createMeta() writes its dataSetId/filterId from
	/// here and createFilterIds() derives the transient sidecar from here (mds-plan D3.1/D3.2,
	/// no drift between mirror and generator).
	Filter						*	generatedHostFilter()												const;
	///< The host filter's id (see generatedHostFilter), or -1 when the control has no host stamp.
	/// Family generators read provenance through THIS (not the Filter*) so they need no dependency
	/// on the Filter class itself.
	int									generatedHostFilterId()											const;

	JASPControl*				_control			= nullptr;
	bool						_isComputedColumn	= false,
								_isColumn			= false;
	std::set<std::string>		_isRCode;
	Json::Value					_orgValue,
								_defaultValue;
	columnType					_columnType			= columnType::unknown;
	std::string					_filterForValues;
};

#endif // BOUNDCONTROLBASE_H
