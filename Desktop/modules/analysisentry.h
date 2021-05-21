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


#ifndef ANALYSISENTRY_H
#define ANALYSISENTRY_H

#include <string>
#include <vector>
#include "jsonredirect.h"
#include <functional>

namespace Modules
{
class DynamicModule;
class EntryBase;
class AnalysisEntry;
typedef std::vector<AnalysisEntry*> AnalysisEntries;

class AnalysisEntry
{
	friend EntryBase;
public:
	AnalysisEntry(std::function<void()> specialFunc, std::string menuTitle, bool requiresData=true, std::string icon = "");
	AnalysisEntry(Json::Value & analysisEntry, DynamicModule * dynamicModule, bool defaultRequiresData = true);
	AnalysisEntry();

	std::string		menu()					const { return _menu;									}
	std::string		title()					const { return _title;									}
	std::string		function()				const { return _function;								}
	std::string		qml()					const { return _qml != "" ? _qml : _function + ".qml";	}
	std::string		icon()					const;
	bool			isSeparator()			const { return _isSeparator;		}
	bool			isGroupTitle()			const { return _isGroupTitle;		}
	bool			isAnalysis()			const { return _isAnalysis;			}
	bool			isEnabled()				const { return _isEnabled;			}
	bool			requiresData()			const { return _requiresData;		}
	bool			shouldBeExposed()		const { return _isAnalysis && !_isSeparator && _function != "???"; }

	DynamicModule*	dynamicModule()			const;
	std::string		qmlFilePath()			const;
	std::string		getFullRCall()			const;
	Json::Value		getDefaultResults()		const;
	Json::Value		asJsonForJaspFile()		const;

	std::string		codedReference()		const;
	std::string		buttonMenuString()		const;

	void			runSpecialFunc()		const { _specialFunc(); }

	static bool		requiresDataEntries(const AnalysisEntries & entries);

private:
	std::string				_title			= "???"		,
							_function		= "???"		,
							_qml			= "???"		,
							_menu			= "???"		;
	DynamicModule*			_dynamicModule	= nullptr	;
	bool					_isSeparator	= true		,
							_isGroupTitle	= false		,
							_isAnalysis		= false		,
							_isEnabled		= true		,
							_requiresData	= true		;
	std::string				_icon			= ""		;
	std::function<void()>	_specialFunc	= nullptr;
};

}

#endif // ANALYSISENTRY_H
