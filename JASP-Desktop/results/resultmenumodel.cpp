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

#include "results/resultmenumodel.h"




QVariant ResultMenuModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	ResultMenuEntry entry = _resultMenuEntries.at(size_t(index.row()));

	if		(role == DisplayRole)
		return entry.displayText();
	else if	(role == NameRole)
		return entry.name();
	else if	(role == MenuImageSourceRole)
		return entry.menuImageSource();
	else if	(role == JSFunctionRole)
		return entry.jsFunction();
	else if (role == IsSeparatorRole)
		return entry.isSeparator();

	return QVariant();
}


QHash<int, QByteArray> ResultMenuModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray> {
		{	DisplayRole,			"displayText"		},
		{	NameRole,				"name"				},
		{	MenuImageSourceRole,	"menuImageSource"	},
		{	JSFunctionRole,			"jsFunction"		},
		{	IsSeparatorRole,		"isSeparator"		}
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
	int numEntries = ResultMenuEntry::EntriesOrder.size();

	for (int i = 0; i < numEntries; ++i) {
		QString key = ResultMenuEntry::EntriesOrder.at(i);

		if (!selected.contains(key))
			continue;

		ResultMenuEntry entry = ResultMenuEntry::AllResultEntries.find(key)->second;

		if (key == "hasCollapse") {
			Json::Value collapseOptions = menuOptions["collapseOptions"];

			QString iconPath = collapseOptions["collapsed"].asBool() ? "expand" : "collapse";
			entry.setImageSource("qrc:/icons/" + iconPath + ".png");

			QString displayText = QString::fromStdString(collapseOptions["menuText"].asString());
			entry.setDisplayText(displayText);

		} else if (key == "hasNotes") {

			entries.push_back(separator);
			Json::Value noteOptions = menuOptions["noteOptions"];

			for (Json::ValueIterator iter = noteOptions.begin(); iter != noteOptions.end(); iter++) {

				Json::Value noteOption = *iter;
				entry.setDisplayText(QString::fromStdString(noteOption["menuText"].asString()));

				QString jsFunction = QString("window.notesMenuClicked('%1', %2);").arg(QString::fromStdString(noteOption["key"].asString())).arg(noteOption["visible"].asBool() ? "false" : "true");
				entry.setJSFunction(jsFunction);

				entries.push_back(entry);
			}
			continue;
		}

		entries.push_back(entry);
	}

	_resultMenuEntries = entries;
	endResetModel();
}
