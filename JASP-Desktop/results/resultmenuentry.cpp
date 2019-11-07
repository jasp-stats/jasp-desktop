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

#include "resultmenuentry.h"
#include "qquick/jasptheme.h"

//the window.blabla() functions are defined main.js and the hasBlabla are defined jaspwidgets.js
std::map<QString, ResultMenuEntry> ResultMenuEntry::AllResultEntries = {
	{	"hasCollapse",				ResultMenuEntry("",						"hasCollapse",				"",							"window.collapseMenuClicked();")	},
	{	"hasEditTitle",				ResultMenuEntry("Edit Title",			"hasEditTitle",				"edit-pencil.png",			"window.editTitleMenuClicked();")	},
	{	"hasCopy",					ResultMenuEntry("Copy",					"hasCopy",					"copy.png",					"window.copyMenuClicked();")		},
	{	"hasLaTeXCode",				ResultMenuEntry("Copy LaTeX",			"hasLaTeXCode",				"code-icon.png",			"window.latexCodeMenuClicked();")	},
	{	"hasCite",					ResultMenuEntry("Copy Citations",		"hasCite",					"cite.png",					"window.citeMenuClicked();")		},
	{	"hasSaveImg",				ResultMenuEntry("Save Image As",		"hasSaveImg",				"document-save-as.png",		"window.saveImageClicked();")		},
	{	"hasEditImg",				ResultMenuEntry("Edit Image",			"hasEditImage",				"editImage.png",			"window.editImageClicked();")		},
	{	"hasNotes",					ResultMenuEntry("Add Note",				"hasNotes",					"",							"")									},
	{	"hasDuplicate",				ResultMenuEntry("Duplicate",			"hasDuplicate",				"duplicate.png",			"window.duplicateMenuClicked();")	},
	{	"hasRemove",				ResultMenuEntry("Remove",				"hasRemove",				"close-button.png",			"window.removeMenuClicked();")		},
	{	"hasRemoveAllAnalyses",		ResultMenuEntry("Remove All",			"hasRemoveAllAnalyses",		"close-button.png",			"")									},
	{	"hasRefreshAllAnalyses",	ResultMenuEntry("Refresh All",			"hasRefreshAllAnalyses",	"",							"")									},
	{	"hasShowDeps",				ResultMenuEntry("Show Dependencies",	"hasShowDeps",				"",							"window.showDependenciesClicked()")	}
};

QStringList ResultMenuEntry::EntriesOrder = {"hasCollapse", "hasEditTitle", "hasCopy", "hasLaTeXCode", "hasCite", "hasSaveImg",
											 "hasEditImg", "hasNotes", "hasDuplicate", "hasRemove", "hasRemoveAllAnalyses", "hasRefreshAllAnalyses", "hasShowDeps"};

ResultMenuEntry::ResultMenuEntry(QString displayText, QString name, QString menuImageSource, QString jsFunction)
	: _displayText(displayText)
	, _name(name)
	, _menuImageSource(menuImageSource)
	, _jsFunction(jsFunction)
	, _isSeparator(false)
	, _isEnabled(true)
{
}

ResultMenuEntry::ResultMenuEntry()
{
	_isSeparator = true;
}

QString	ResultMenuEntry::menuImageSource() const
{

	return _menuImageSource == "" ? "" : JaspTheme::currentIconPath() + _menuImageSource;
}
