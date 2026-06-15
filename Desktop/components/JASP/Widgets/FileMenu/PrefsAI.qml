import QtQuick
import QtQuick.Controls as QTC
import QtQuick.Dialogs as QTD
import JASP.Widgets
import JASP.Controls


PrefsScrollView
{
	id:						scrollPrefs

	MenuHeader
	{
		id:				menuHeader
		headertext:		qsTr("AI Settings")
		helpfile:		"preferences/PrefsAI"
		addMargin:		false
	}

	Item
	{
		Connections
		{
			target:			aiBridge
			function onTestConnectionResult(success, message)
			{
				testResultLabel.text = message
				testResultLabel.color = success ? jaspTheme.jaspGreen : jaspTheme.redDarker
			}
		}
	}

	Item
	{
		Connections
		{
			target:			preferencesModel.aiPersonaModel
			function onCurrentPersonaIndexChanged() { personaTabBar.currentIndex = preferencesModel.aiPersonaModel.currentPersonaIndex }
			function onCountChanged() {
				if (preferencesModel.aiPersonaModel.count > 0 && personaTabBar.currentIndex >= preferencesModel.aiPersonaModel.count)
					personaTabBar.currentIndex = 0
			}
		}
	}

	PrefsGroupRect
	{
		title:				qsTr("Connection")
		visible:			preferencesModel.aiEnabled

		// CheckBox
		// {
		// 	id:				customKeyCheck
		// 	label:			qsTr("Use custom API key / endpoint")
		// 	checked:		preferencesModel.aiUseCustomKey
		// 	onCheckedChanged:	preferencesModel.aiUseCustomKey = checked
		// 	visible:		false
		// 	focus:			true
		// 	KeyNavigation.tab:	aiEndpointInput
		// }

		Text
		{
			id:				labelMeasure
			text:			qsTr("Endpoint URL:")
			font:			jaspTheme.font
			visible:		false
		}

		Item
		{
			width:			parent.width
			height:			aiEndpointInput.height

			Label
			{
				id:					aiEndpointLabel
				text:				qsTr("Endpoint URL:")
				width:				labelMeasure.implicitWidth + jaspTheme.generalAnchorMargin

				anchors
				{
					left:			parent.left
					verticalCenter:	parent.verticalCenter
				}
			}

			PrefsTextInput
			{
				id:					aiEndpointInput
				text:				preferencesModel.aiEndpoint
				onEditingFinished:	preferencesModel.aiEndpoint = text
				nextEl:				aiApiKeyInput
				focus:				true

				anchors
				{
					left:			aiEndpointLabel.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
				}

				KeyNavigation.tab:	aiApiKeyInput
			}
		}

		Item
		{
			width:			parent.width
			height:			aiApiKeyInput.height

			Label
			{
				id:					aiApiKeyLabel
				text:				qsTr("API Key:")
				width:				labelMeasure.implicitWidth + jaspTheme.generalAnchorMargin

				anchors
				{
					left:			parent.left
					verticalCenter:	parent.verticalCenter
				}
			}

			PrefsTextInput
			{
				id:					aiApiKeyInput
				text:				preferencesModel.aiApiKey
				onEditingFinished:	preferencesModel.aiApiKey = text
				nextEl:				aiModelInput
				textInput.echoMode:	TextInput.Password

				anchors
				{
					left:			aiApiKeyLabel.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
				}
			}
		}

		Item
		{
			width:			parent.width
			height:			aiModelInput.height

			Label
			{
				id:					aiModelLabel
				text:				qsTr("Model:")
				width:				labelMeasure.implicitWidth + jaspTheme.generalAnchorMargin

				anchors
				{
					left:			parent.left
					verticalCenter:	parent.verticalCenter
				}
			}

			PrefsTextInput
			{
				id:					aiModelInput
				text:				preferencesModel.aiModel
				onEditingFinished:	preferencesModel.aiModel = text
				nextEl:				personaTabBar

				anchors
				{
					left:			aiModelLabel.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
				}
			}
		}

		Item
		{
			width:			parent.width
			height:			testButton.height + testResultLabel.height + jaspTheme.generalAnchorMargin

			RectangularButton
			{
				id:				testButton
				text:			qsTr("Test Connection")
				toolTip:		qsTr("Send a minimal request to verify your endpoint and API key.")
				anchors.left:	parent.left
				anchors.top:	parent.top
				anchors.topMargin: jaspTheme.generalAnchorMargin

				onClicked:		{
					testResultLabel.text = qsTr("Testing…")
					testResultLabel.color = jaspTheme.textEnabled
					aiBridge.testConnection()
				}
			}

			Text
			{
				id:				testResultLabel
				text:			""
				font:			jaspTheme.font
				color:			jaspTheme.textEnabled
				wrapMode:		Text.WordWrap
				anchors
				{
					left:		testButton.right
					right:		parent.right
					verticalCenter: testButton.verticalCenter
					leftMargin:	jaspTheme.generalAnchorMargin
				}
			}
		}
	}


	PrefsGroupRect
	{
		title: qsTr("Personas")
		visible: preferencesModel.aiEnabled

		// --- Tab bar ---
		QTC.ScrollView
		{
			id: tabScrollView
			width: parent.width
			height: 36 * preferencesModel.uiScale
			QTC.ScrollBar.horizontal.policy: QTC.ScrollBar.AlwaysOn

			QTC.TabBar
			{
				id: personaTabBar
				contentHeight: 32 * preferencesModel.uiScale

			background: Rectangle { color: jaspTheme.uiBackground }

			Repeater
			{
				model: preferencesModel.aiPersonaModel
				QTC.TabButton
				{
					text: personaName
					font: jaspTheme.font
					width: Math.max(implicitWidth, 90 * preferencesModel.uiScale)

					contentItem: Text
					{
						text: personaName
						font: jaspTheme.font
						color: jaspTheme.black
						horizontalAlignment: Text.AlignHCenter
						verticalAlignment: Text.AlignVCenter
						elide: Text.ElideRight
					}

					background: Rectangle
					{
						color: checked ? jaspTheme.grayLighter : jaspTheme.uiBackground
						border.color: checked ? jaspTheme.uiBorder : jaspTheme.borderColor
						border.width: 1
					}

					Rectangle
					{
						anchors.right: parent.right
						anchors.rightMargin: 4
						anchors.verticalCenter: parent.verticalCenter
						width:  8 * preferencesModel.uiScale
						height: width
						radius: width / 2
						color: preferencesModel.aiPersonaModel.currentPersonaIndex === index
						       ? jaspTheme.jaspGreen : "transparent"
					}
				}
			}

			QTC.TabButton
			{
				width: 32 * preferencesModel.uiScale

				contentItem: Text
				{
					text: "+"
					font: jaspTheme.font
					color: jaspTheme.black
					horizontalAlignment: Text.AlignHCenter
					verticalAlignment: Text.AlignVCenter
				}

				background: Rectangle { color: jaspTheme.grayLighter; border.color: jaspTheme.borderColor; border.width: 1 }
				onClicked: { preferencesModel.aiPersonaModel.addPersona(); personaTabBar.currentIndex = personaTabBar.count - 1; }
			}

			onCurrentIndexChanged: {
				if (currentIndex >= 0 && currentIndex < preferencesModel.aiPersonaModel.count)
					personaEditorColumn.refreshEditor()
			}
		}
	}

	// --- Editor panel ---
		Column
		{
			id: personaEditorColumn
			width: parent.width
			visible: preferencesModel.aiPersonaModel.count > 0
			spacing: jaspTheme.rowSpacing

			// Role shortcuts (used throughout)
			readonly property int nameR:   Qt.UserRole + 2
			readonly property int promptR: Qt.UserRole + 3
			readonly property int imageR:  Qt.UserRole + 4
			readonly property int sysR:    Qt.UserRole + 5
			readonly property int toolsR:  Qt.UserRole + 6

			function modelData(role) {
				var idx = personaTabBar.currentIndex
				if (idx < 0 || idx >= preferencesModel.aiPersonaModel.count) return ""
				return preferencesModel.aiPersonaModel.data(preferencesModel.aiPersonaModel.index(idx, 0), role)
			}

			function refreshEditor() {
				personaNameField.text        = modelData(nameR)
				personaNameField.enabled     = true
				personaPromptInput.text     = modelData(promptR)
				personaPromptInput.readOnly = false
				personaImageBrowse.enabled   = true
				personaDeleteBtn.visible     = !modelData(sysR)
				personaResetBtn.visible      = modelData(sysR)
				personaImagePreview.source   = preferencesModel.aiPersonaModel.resolvedImageUrl(modelData(imageR))

				// Initial checkbox state (signal hasn't fired yet)
				var enabled = modelData(toolsR)
				var isDefault = (enabled.length === 0 || (enabled.length === 1 && (enabled[0] === '*' || enabled[0] === '_default_')))
				var effective = isDefault ? preferencesModel.aiPersonaModel.effectiveEnabledTools(personaTabBar.currentIndex) : enabled
				for (var i = 0; i < toolRepeater.count; i++) {
					var cb = toolRepeater.itemAt(i)
					if (cb) cb.checked = (effective.indexOf(cb.toolName) >= 0)
				}

				// Same for caps: set explicitly from model
				var capIds = preferencesModel.aiPersonaModel.enabledCapabilityIds(personaTabBar.currentIndex)
				for (var c = 0; c < capRepeater.count; c++) {
					var capCb = capRepeater.itemAt(c)
					if (capCb) capCb.checked = (capIds.indexOf(capCb.capId) >= 0)
				}
			}

			// Initial sync — no signal fires on load
			Component.onCompleted: {
				personaTabBar.currentIndex = preferencesModel.aiPersonaModel.currentPersonaIndex
				personaEditorColumn.refreshEditor()
			}

			// React to model-data changes
			Connections {
				target: preferencesModel.aiPersonaModel
				function onPersonaNameChanged(index, name) {
					if (index === personaTabBar.currentIndex) personaNameField.text = name
				}
				function onPersonaPromptChanged(index, prompt) {
					if (index === personaTabBar.currentIndex) personaPromptInput.text = prompt
				}
				function onPersonaImagePathChanged(index, path) {
					if (index === personaTabBar.currentIndex)
						personaImagePreview.source = preferencesModel.aiPersonaModel.resolvedImageUrl(path)
				}
				function onPersonaEnabledToolsChanged(index, tools) {
					var isDefault = (tools.length === 0 || (tools.length === 1 && (tools[0] === '*' || tools[0] === '_default_')))
					var effective = isDefault ? preferencesModel.aiPersonaModel.effectiveEnabledTools(index) : tools
					for (var i = 0; i < toolRepeater.count; i++) {
						var cb = toolRepeater.itemAt(i)
						if (cb) cb.checked = (effective.indexOf(cb.toolName) >= 0)
					}
				}
			}

			// --- Name ---
			Item {
				width: parent.width
				height: personaNameField.height

				Label {
					text: qsTr("Name:")
					font: jaspTheme.font
					anchors.verticalCenter: parent.verticalCenter
					anchors.left: parent.left
					width: 80 * preferencesModel.uiScale
				}

				PrefsTextInput {
					id: personaNameField
					width: parent.width - x
					anchors.leftMargin: 0
					x: 80 * preferencesModel.uiScale
					onEditingFinished: preferencesModel.aiPersonaModel.setData(
						preferencesModel.aiPersonaModel.index(personaTabBar.currentIndex, 0),
						text, personaEditorColumn.nameR)
				}
			}

			// --- Avatar ---
			Item {
				width: parent.width
				height: Math.max(personaImagePreview.height, personaImageBrowse.height)

				Label {
					text: qsTr("Avatar:")
					font: jaspTheme.font
					anchors.verticalCenter: parent.verticalCenter
					anchors.left: parent.left
					width: 80 * preferencesModel.uiScale
				}

				Image {
					id: personaImagePreview
					x: 80 * preferencesModel.uiScale
					anchors.verticalCenter: parent.verticalCenter
					width: 40 * preferencesModel.uiScale
					height: width
					fillMode: Image.PreserveAspectCrop
					sourceSize.width: 80
					sourceSize.height: 80
				}

				RectangularButton {
					id: personaImageBrowse
					text: qsTr("Browse")
					x: personaImagePreview.x + personaImagePreview.width + jaspTheme.generalAnchorMargin
					anchors.verticalCenter: parent.verticalCenter
					enabled: true
					onClicked: personaImageFileDialog.open()
				}

				QTD.FileDialog {
					id: personaImageFileDialog
					title: qsTr("Select Persona Image")
					nameFilters: [qsTr("Images (*.png *.jpg *.jpeg *.gif *.svg)")]
					onAccepted: {
						var path = preferencesModel.aiPersonaModel.copyImageToPersonasDir(selectedFile)
						if (path)
							preferencesModel.aiPersonaModel.setData(
								preferencesModel.aiPersonaModel.index(personaTabBar.currentIndex, 0),
								path, personaEditorColumn.imageR)
					}
				}
			}

			// --- Persona Prompt ---
			Label {
				text: qsTr("Persona Prompt:")
				font: jaspTheme.font
				color: jaspTheme.textEnabled
			}

			Rectangle {
				border.color: jaspTheme.borderColor
				border.width: 1
				radius: jaspTheme.borderRadius
				color: jaspTheme.white
				width: parent.width
				height: 120 * preferencesModel.uiScale

				QTC.ScrollView {
					anchors.fill: parent
					anchors.margins: 1

					QTC.TextArea {
						id: personaPromptInput
						font: jaspTheme.font
						color: jaspTheme.textEnabled
						wrapMode: TextEdit.Wrap
						selectByMouse: true
						onEditingFinished: preferencesModel.aiPersonaModel.setData(
							preferencesModel.aiPersonaModel.index(personaTabBar.currentIndex, 0),
							text, personaEditorColumn.promptR)
					}
				}
			}

			// --- Tools ---
			Section {
				title: qsTr("Persona Capabilities")

				Column {
					width: parent.width
					spacing: jaspTheme.rowGridSpacing

					// ---- Capabilities ----
					Item {
						id: capsContainer
						width: parent.width
						height: capsGrid.height

						property var capsData: preferencesModel.aiPersonaModel.capabilities()

						Connections {
							target: preferencesModel.aiPersonaModel
							onPersonaEnabledCapabilitiesChanged: {
								var ids = preferencesModel.aiPersonaModel.enabledCapabilityIds(personaTabBar.currentIndex)
								for (var c = 0; c < capRepeater.count; c++) {
									var cb = capRepeater.itemAt(c)
									if (cb) cb.checked = (ids.indexOf(cb.capId) >= 0)
								}
							}
						}

						Grid {
							id: capsGrid
							width: parent.width
							columns: 2
							spacing: 4 * preferencesModel.uiScale

							Repeater {
								id: capRepeater
								model: capsContainer.capsData

								CheckBox {
									property string capId: modelData.id
									label: modelData.displayName
									enabled: modelData.methods.length > 0
									width: capsGrid.width / 2 - capsGrid.spacing

									onClicked: {
										preferencesModel.aiPersonaModel.toggleCapability(personaTabBar.currentIndex, capId)
										checked = preferencesModel.aiPersonaModel.enabledCapabilityIds(personaTabBar.currentIndex).indexOf(capId) >= 0
									}
								}
							}
						}
					}

					// ---- Advanced (individual tools) ----
					Section {
						title: qsTr("Advanced")
						expanded: false

						Flow {
							width: parent.width
							spacing: 4 * preferencesModel.uiScale

							Repeater {
								id: toolRepeater
								model: preferencesModel.aiPersonaModel.allKnownToolNames()

								CheckBox {
									property string toolName: modelData
									label: preferencesModel.aiPersonaModel.toolDisplayName(modelData)

									onClicked: preferencesModel.aiPersonaModel.toggleTool(personaTabBar.currentIndex, toolName)
								}
							}
						}
					}
				}
			}

			// --- Actions ---
			Row {
				id: personaActionsRow
				spacing: jaspTheme.generalAnchorMargin

				RectangularButton {
					text: qsTr("Set as Active")
					onClicked: preferencesModel.aiPersonaModel.currentPersonaIndex = personaTabBar.currentIndex
					enabled: preferencesModel.aiPersonaModel.currentPersonaIndex !== personaTabBar.currentIndex
				}

				RectangularButton {
					text: qsTr("Duplicate")
					onClicked: preferencesModel.aiPersonaModel.duplicatePersona(personaTabBar.currentIndex)
				}

				RectangularButton {
					id: personaDeleteBtn
					text: qsTr("Delete Persona")
					onClicked: preferencesModel.aiPersonaModel.removePersona(personaTabBar.currentIndex)
				}

				RectangularButton {
					id: personaResetBtn
					text: qsTr("Reset to Default")
					visible: false
					onClicked: preferencesModel.aiPersonaModel.resetSystemPersona(personaTabBar.currentIndex)
				}
			}
		}
	}


		PrefsGroupRect
	{
		title:				qsTr("Additional Parameters")
		visible:			preferencesModel.aiEnabled

		CheckBox
		{
			id:					completeSchemaCheck
			label:				qsTr("Include full tool schemas in request")
			checked:			preferencesModel.aiUseCompleteSchema
			onCheckedChanged:	preferencesModel.aiUseCompleteSchema = checked
			toolTip:			qsTr(
				"When enabled, each tool in the API request includes its full "
				+ "parameter schema (with JSON types such as integer/Boolean). "
				+ "This helps models that struggle with type-safety in tool "
				+ "calls (e.g., Qwen). Uses more tokens. Leave unticked for DeepSeek."
			)
		}

		Label
		{
			text:			qsTr("Common System Prompt:")
			font:			jaspTheme.font
			color:			jaspTheme.textEnabled
			wrapMode:		Text.WordWrap
			width:			parent.width
		}

		Rectangle
		{
			border.color:	jaspTheme.borderColor
			border.width:	1
			radius:			jaspTheme.borderRadius
			color:			jaspTheme.white
			width:			parent.width
			height:			80 * preferencesModel.uiScale

			QTC.ScrollView
			{
				anchors.fill:		parent
				anchors.margins:	1

				QTC.TextArea
				{
					id:				aiCommonSystemPromptInput
					text:			preferencesModel.aiCommonSystemPrompt
					font:			jaspTheme.font
					color:			jaspTheme.textEnabled
					wrapMode:		TextEdit.Wrap
					selectByMouse:	true
					placeholderText: qsTr("Common system prompt shared across all personas…")

						onEditingFinished:	preferencesModel.aiCommonSystemPrompt = text
					}
				}
			}

			Item {
				width: chatLimitCheck.width + 120 * preferencesModel.uiScale
				height: chatLimitCheck.height

				CheckBox {
					id: chatLimitCheck
					label: qsTr("Single chat token limit:")
					checked: preferencesModel.aiChatLimitActive
					onCheckedChanged: preferencesModel.aiChatLimitActive = checked
				}

				IntegerField {
					id: chatLimitField
					value: preferencesModel.aiChatLimit
					onValueChanged: preferencesModel.aiChatLimit = value
					enabled: chatLimitCheck.checked
					fieldWidth: 100 * preferencesModel.uiScale
					toolTip: qsTr("~4 characters ≈ 1 token")

					anchors {
						left: chatLimitCheck.right
						leftMargin: jaspTheme.generalAnchorMargin
						verticalCenter: chatLimitCheck.verticalCenter
					}
				}
			}

			Label
			{
				text:			qsTr("Paste a JSON object with extra parameters to include in every API request.\nExamples: { \"max_tokens\": 4096, \"thinking\": { \"type\": \"enabled\" } }\nFields \"model\", \"stream\", \"messages\", \"tools\", and \"text\" are protected and will be ignored.")
			font:			jaspTheme.font
			color:			jaspTheme.textEnabled
			wrapMode:		Text.WordWrap
			width:			parent.width
		}

		Rectangle
		{
			border.color:	jaspTheme.borderColor
			border.width:	1
			radius:			jaspTheme.borderRadius
			color:			jaspTheme.white
			width:			parent.width
			height:			150 * preferencesModel.uiScale

			QTC.ScrollView
			{
				anchors.fill:		parent
				anchors.margins:	1

				QTC.TextArea
				{
					id:				aiExtraParamsInput
					text:			preferencesModel.aiExtraParams
					font:			jaspTheme.font
					color:			jaspTheme.textEnabled
					wrapMode:		TextEdit.Wrap
					selectByMouse:	true

					onEditingFinished:	preferencesModel.aiExtraParams = text
				}
			}
		}
	}


	PrefsGroupRect
	{
		title:				qsTr("Per-Message Extra Fields")
		visible:			preferencesModel.aiEnabled

		Label
		{
			text:			qsTr(
				"Paste a JSON object to merge into every message of the API request.\n"
				+ "Use this for per-message features like explicit caching: { \"cache_control\": { \"type\": \"ephemeral\" } }\n"
				+ "Fields \"role\", \"content\", and \"text\" are protected and will be ignored."
			)
			font:			jaspTheme.font
			color:			jaspTheme.textEnabled
			wrapMode:		Text.WordWrap
			width:			parent.width
		}

		Rectangle
		{
			border.color:	jaspTheme.borderColor
			border.width:	1
			radius:			jaspTheme.borderRadius
			color:			jaspTheme.white
			width:			parent.width
			height:			120 * preferencesModel.uiScale

			QTC.ScrollView
			{
				anchors.fill:		parent
				anchors.margins:	1

				QTC.TextArea
				{
					id:				aiMessageExtraInput
					text:			preferencesModel.aiMessageExtra
					font:			jaspTheme.font
					color:			jaspTheme.textEnabled
					wrapMode:		TextEdit.Wrap
					selectByMouse:	true
					placeholderText: qsTr("{ \"cache_control\": { \"type\": \"ephemeral\" } }")

					onEditingFinished:	preferencesModel.aiMessageExtra = text
				}
			}
		}
	}

	RectangularButton
	{
		visible:		preferencesModel.aiEnabled
		text:			qsTr("Reset all AI settings to defaults")
		toolTip:		qsTr("Restore endpoint, model, system prompt, and all other AI settings to their original defaults.")
		onClicked:		preferencesModel.resetAiDefaults()
		anchors.left:	parent.left
	}

	PrefsGroupRect
	{
		title:				qsTr("MCP")
		visible:			preferencesModel.aiEnabled

		CheckBox
		{
			id:					mcpEnabled
			label:				qsTr("Enable MCP server (Model Context Protocol)")
			checked:			preferencesModel.rpcServerEnabled
			onCheckedChanged:	preferencesModel.rpcServerEnabled = checked
			toolTip:			qsTr("Enable MCP server for AI model context protocol. To change the port or bind IP address, see Advanced > Remote control.")
		}
	}

	PrefsGroupRect
	{
		title:				qsTr("AI Service")

		RectangularButton
		{
			id:				aiEnableBtn
			text:			preferencesModel.aiEnabled ? qsTr("Disable") : qsTr("Enable")
			toolTip:		qsTr("Toggle AI functionality. A confirmation dialog will appear when enabling.")
			anchors.left:	parent.left

			onClicked: {
				if (preferencesModel.aiEnabled) {
					preferencesModel.aiEnabled = false
				} else {
					var agreed = messages.showYesNoQML(
						qsTr("Before using JASP AI"),
						qsTr("JASP AI can help you choose, conduct, interpret, and report statistical analyses. AI responses and actions may be incorrect, incomplete, or inappropriate for your data, so always verify important statistical decisions, results, assumptions, and conclusions independently.\n\nDuring use, JASP AI may add, change, or replace analyses in your current project. To avoid losing work, we recommend saving a backup copy of your JASP file, and where relevant your original data file, before using JASP AI.\n\nInformation from your data set, analyses, output, and chat messages may be processed by the AI service to answer your questions. Do not use sensitive, confidential, or restricted data unless you are allowed to share it."),
						qsTr("I Agree"),
						qsTr("Cancel"))
					if (agreed) preferencesModel.aiEnabled = true
				}
			}
		}
	}
}
