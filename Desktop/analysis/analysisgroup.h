//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef ANALYSISGROUP_H
#define ANALYSISGROUP_H

#include <string>
#include "analysisbase.h"

/// A named group header that can be placed between analyses in the left panel.
/// Groups are purely organisational: they act as collapsible dividers.
class AnalysisGroup
{
public:
					AnalysisGroup(size_t id, const std::string & title = "Group");

	size_t			id()		const	{ return _id;		}
	std::string		title()		const	{ return _title;	}
	bool			collapsed()	const	{ return _collapsed;}

	void			setTitle(const std::string & title)	{ _title = title;		}
	void			setCollapsed(bool collapsed)		{ _collapsed = collapsed; }

	Json::Value		asJson() const;
	static AnalysisGroup * fromJson(const Json::Value & json);

private:
	size_t			_id;
	std::string		_title;
	bool			_collapsed	= false;
};

#endif // ANALYSISGROUP_H
