//
// Copyright (C) 2013-2022 University of Amsterdam
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

import QtQuick
import JASP.Widgets
import QtQuick.Controls as QC
import JASP.Controls
import QtQuick.Layouts as LQ
import JASP

Item
{
	id	: rect


	MouseArea
	{
		z:				-5
		anchors.fill:	parent
		onClicked:		rect.forceActiveFocus()
	}

	MenuHeader
	{
		id				: menuHeader
		headertext		: qsTr("Database")
	}
	
	Text
	{
		text:		qsTr(
"<i>Warning!</i>

JASP stores the password to your database in jasp-files, while not directly readable it is easy to extract.

In case you are trying to connect to a production or even just network-accessible database:
We <b>strongly urge</b> you to make a special user for JASP in your database with as <i>few permissions</i> as needed.

For a local or toy database this is probably overkill, but use your own judgement."
)
				
	}

	PrefsGroupRect
	{
		id:		databaseGroup

		title:	qsTr("Database")

		anchors.top: menuHeader.bottom

		ComboBox
		{
			id:						dbDriver
			fieldWidth:				400


			label:					qsTr("Choose DB driver")

			currentIndex:			fileMenuModel.database.dbType
			onCurrentIndexChanged:	fileMenuModel.database.setDbTypeFromIndex(currentIndex);

			source:					fileMenuModel.database.dbTypes 

			KeyNavigation.tab:		dbHostnameInput
		}

		RowLayout
		{
			width:					parent.width

			Text
			{
				id:				dbHostnameLabel
				text:			qsTr("Hostname")
				width:			jaspTheme.generalAnchorMargin + Math.max(contentWidth, dbPortLabel.contentWidth, dbNameLabel.contentWidth, dbUsernameLabel.contentWidth, dbPasswordLabel.contentWidth)
			}

			PrefsTextInput
			{
				id:				dbHostnameInput
				nextEl:			dbPortInput.textInput
				text:			fileMenuModel.database.hostname
				onTextChanged:	fileMenuModel.database.hostname = text;

				LQ.Layout.fillWidth:	true
			}
		}

		RowLayout
		{
			width:					parent.width

			Text
			{
				id:				dbPortLabel
				text:			qsTr("Port")
				width:			dbHostnameLabel.width
			}

			PrefsTextInput
			{
				id:				dbPortInput
				nextEl:			dbNameInput.textInput
				text:			fileMenuModel.database.port
				onTextChanged:	fileMenuModel.database.port = parseInt(text);

				textInput.validator: JASPDoubleValidator { id: intValidator; bottom: 0; top: 9999999999999; decimals: 0 }

				LQ.Layout.fillWidth:	true
			}
		}

		RowLayout
		{
			width:					parent.width

			Text
			{
				id:				dbNameLabel
				text:			qsTr("Name")
				width:			dbHostnameLabel.width
			}

			PrefsTextInput
			{
				id:				dbNameInput
				nextEl:			dbUsernameInput.textInput
				text:			fileMenuModel.database.database
				onTextChanged:	fileMenuModel.database.database = text;

				LQ.Layout.fillWidth:	true
			}
		}

		RowLayout
		{
			width:					parent.width

			Text
			{
				id:				dbUsernameLabel
				text:			qsTr("Username")
				width:			dbHostnameLabel.width
			}

			PrefsTextInput
			{
				id:				dbUsernameInput
				nextEl:			dbPasswordInput.textInput
				text:			fileMenuModel.database.username
				onTextChanged:	fileMenuModel.database.username = text;

				LQ.Layout.fillWidth:	true
			}
		}

		RowLayout
		{
			width:					parent.width

			Text
			{
				id:				dbPasswordLabel
				text:			qsTr("Password")
				width:			dbHostnameLabel.width
			}

			PrefsTextInput
			{
				id:						dbPasswordInput
				nextEl:					connectButton
				text:					fileMenuModel.database.password
				onTextChanged:			fileMenuModel.database.password = text;

				textInput.echoMode:		TextInput.Password

				LQ.Layout.fillWidth:	true
			}
		}

		RoundedButton
		{
			id:					connectButton
			text:				qsTr("Connect to database")
			onClicked:			fileMenuModel.database.connect();
			KeyNavigation.tab:	dbQuery.textInput
		}


		RowLayout
		{
			width:						parent.width

			enabled:					fileMenuModel.database.connected

			Text
			{
				id:						dbQueryLabel
				text:					qsTr("Query")
				width:					implicitWidth + jaspTheme.generalAnchorMargin
			}

			PrefsTextInput
			{
				id:						dbQuery
				nextEl:					runQuery
				text:					fileMenuModel.database.query
				onTextChanged:			fileMenuModel.database.query = text
				LQ.Layout.fillWidth:	true
			}

			RoundedButton
			{
				id:						runQuery
				text:					qsTr("Execute")
				onClicked:				fileMenuModel.database.runQuery();
				KeyNavigation.tab:	loadResults
			}
		}

	}

	PrefsGroupRect
	{
		id:			previewGroup
		title:		qsTr("Preview")

		anchors.top:		databaseGroup.bottom
		anchors.topMargin:	jaspTheme.generalAnchorMargin
		
		RoundedButton
		{
			id:						loadResults
			text:					qsTr("Load into JASP")
			onClicked:				fileMenuModel.database.importResults();
			enabled:				fileMenuModel.database.connected && fileMenuModel.database.resultsOK
		}

		QC.TextArea
		{
			height:			contentHeight < 100 ? 100 : contentHeight
			text:			fileMenuModel.database.connected ? fileMenuModel.database.queryResult : fileMenuModel.database.lastError
			font:			jaspTheme.font
			color:			jaspTheme.textEnabled
			width:			parent.width
			wrapMode:		TextEdit.Wrap
			readOnly:		true
			selectByMouse:	true
		}
	}
}
