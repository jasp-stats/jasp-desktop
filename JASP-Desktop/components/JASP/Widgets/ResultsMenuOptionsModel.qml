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

import QtQuick 2.0

ListModel {
	ListElement
	{
		displayText		: "Copy"
		menuImageSource	: "qrc:/icons/copy.png"
		jsFunction		: "window.copyMenuClicked();"
	}
	ListElement
	{
		displayText		: "Copy LateX"
		menuImageSource	: "qrc:/icons/code-icon.png"
		jsFunction		: "window.latexCodeMenuClicked();"
	}
	ListElement
	{
		displayText		: "Copy Citations"
		menuImageSource	: "qrc:/icons/cite.png"
		jsFunction		: "window.citeMenuClicked();"
	}
	ListElement
	{
		displayText		: "???"
	}
	ListElement
	{
		displayText		: "Save Image As"
		menuImageSource	: "qrc:/icons/document-save-as.png"
		jsFunction		: "window.saveImageClicked();"
	}
	ListElement
	{
		displayText		: "Edit Image"
		menuImageSource	: "qrc:/icons/editImage.png"
		jsFunction		: "window.editImageClicked();"
	}
	ListElement
	{
		displayText		: "???"
	}
	ListElement
	{
		displayText		: "Add Note"
		menuImageSource	: ""
		jsFunction		: ""
	}
}
