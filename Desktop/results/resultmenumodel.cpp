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

#include "resultmenumodel.h"
#include "utilities/qutils.h"
#include "utilities/settings.h"
#include "qquick/jasptheme.h"
#include "gui/preferencesmodel.h"

ResultMenuModel::ResultMenuModel(QObject *parent) : QAbstractListModel(parent),
	_entriesOrder({"hasCollapse", "hasEditTitle", "hasCopy", "hasLaTeXCode", "hasCite", "hasSaveImg", "hasExportResults",
				"hasEditImg", "hasNotes", "hasDuplicate", "hasRemove", "hasRemoveAllAnalyses", "hasRefreshAllAnalyses", "hasShowDeps"})
{
	_generateCorrectlyTranslatedResultEntries();

	connect(PreferencesModel::prefs(), &PreferencesModel::languageCodeChanged, this, &ResultMenuModel::_generateCorrectlyTranslatedResultEntries);
}

void ResultMenuModel::_generateCorrectlyTranslatedResultEntries()
{
	_allResultEntries =
	{
		{	"hasCollapse",				ResultMenuEntry(tr("Collapse"),				"hasCollapse",				"collapse.png",				"window.collapseMenuClicked();")	},
		{	"hasEditTitle",				ResultMenuEntry(tr("Edit Title"),			"hasEditTitle",				"edit-pencil.png",			"window.editTitleMenuClicked();")	},
		{	"hasCopy",					ResultMenuEntry(tr("Copy"),					"hasCopy",					"copy.png",					"window.copyMenuClicked();")		},
		{	"hasLaTeXCode",				ResultMenuEntry(tr("Copy LaTeX"),			"hasLaTeXCode",				"code-icon.png",			"window.latexCodeMenuClicked();")	},
		{	"hasCite",					ResultMenuEntry(tr("Copy Citations"),		"hasCite",					"cite.png",					"window.citeMenuClicked();")		},
		{	"hasSaveImg",				ResultMenuEntry(tr("Save Image As"),		"hasSaveImg",				"document-save-as.png",		"window.saveImageClicked();")		},
		{	"hasEditImg",				ResultMenuEntry(tr("Edit Image"),			"hasEditImg",				"editImage.png",			"window.editImageClicked();")		},
		{	"hasNotes",					ResultMenuEntry(tr("Add Note"),				"hasNotes",					"",							"")									},
		{	"hasDuplicate",				ResultMenuEntry(tr("Duplicate"),			"hasDuplicate",				"duplicate.png",			"window.duplicateMenuClicked();")	},
		{	"hasRemove",				ResultMenuEntry(tr("Remove"),				"hasRemove",				"close-button.png",			"window.removeMenuClicked();")		},
		{	"hasRemoveAllAnalyses",		ResultMenuEntry(tr("Remove All"),			"hasRemoveAllAnalyses",		"close-button.png",			"")									},
		{	"hasRefreshAllAnalyses",	ResultMenuEntry(tr("Refresh All"),			"hasRefreshAllAnalyses",	"",							"")									},
		{	"hasShowDeps",				ResultMenuEntry(tr("Show Dependencies"),	"hasShowDeps",				"",							"window.showDependenciesClicked()")	},
		{	"hasExportResults",			ResultMenuEntry(tr("Export Results"),		"hasExportResults",			"",							"")									}
   };
}

QVariant ResultMenuModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	ResultMenuEntry entry = _resultMenuEntries.at(size_t(index.row()));

	switch(role)
	{
	case DisplayRole:			return entry.displayText();
	case NameRole:				return entry.name();
	case MenuImageSourceRole:	return entry.menuImageSource();
	case JSFunctionRole:		return entry.jsFunction();
	case IsSeparatorRole:		return entry.isSeparator();
	case IsEnabledRole:			return entry.isEnabled();
	}

	return QVariant();
}


QHash<int, QByteArray> ResultMenuModel::roleNames() const
{

	static const auto roles = QHash<int, QByteArray> {
		{	DisplayRole,			"displayText"		},
		{	NameRole,				"name"				},
		{	MenuImageSourceRole,	"menuImageSource"	},
		{	JSFunctionRole,			"jsFunction"		},
		{	IsSeparatorRole,		"isSeparator"		},
		{	IsEnabledRole,			"isEnabled"			}
	};

	return roles;
}


void ResultMenuModel::setOptions(QString options, QStringList selected)
{
	Json::Value menuOptions;
	Json::Reader parser;
	parser.parse(options.toStdString(), menuOptions);

	beginResetModel();
	std::vector<ResultMenuEntry> entries;

	ResultMenuEntry separator;
	int numEntries = _entriesOrder.size();

	for (int i = 0; i < numEntries; ++i)
	{
		QString key = _entriesOrder.at(i);

		if (!selected.contains(key))
			continue;

		ResultMenuEntry entry = _allResultEntries.find(key)->second;

		if (key == "hasNotes")
		{
			entries.push_back(separator);
			Json::Value noteOptions = menuOptions["noteOptions"];

			for (const Json::Value & noteOption : noteOptions)
			{
				QString jsFunction = QString("window.notesMenuClicked('%1', %2);").arg(tq(noteOption["key"].asString())).arg(noteOption["visible"].asBool() ? "false" : "true");
				entry.setJSFunction(jsFunction);
				entry.setEnabled(!noteOption["visible"].asBool());

				entries.push_back(entry);
			}
		}
		else if (key == "hasShowDeps")
		{
			if(Settings::value(Settings::DEVELOPER_MODE).toBool())
			{
				//It's developerMode time!
				entries.push_back(separator);
				entries.push_back(entry);
			}
		}
		else
				entries.push_back(entry);
	}

	_resultMenuEntries = entries;
	endResetModel();
}
