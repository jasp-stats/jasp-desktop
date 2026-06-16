import QtQuick
import QtQuick.Controls as QTC
import QtQuick.Dialogs as QTD
import QtQuick.Layouts as QTL
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

	PrefsGroupRect
	{
		title:				qsTr("Connection")
		visible:			preferencesModel.aiEnabled

		Group
		{
			id:			connectionId
			columns:	1
			width:		parent.width
			focus:		true

			TextField
			{
				id:					aiEndpointInput
				label:				qsTr("Endpoint URL:")
				value:				preferencesModel.aiEndpoint
				onEditingFinished:	preferencesModel.aiEndpoint = displayValue
				width:				connectionId.width
				fillWidth:			true
				fieldHeight:		25 * jaspTheme.uiScale
				focus:				true
				KeyNavigation.tab:	aiApiKeyInput
			}

			TextField
			{
				id:					aiApiKeyInput
				label:				qsTr("API Key:")
				value:				preferencesModel.aiApiKey
				onEditingFinished:	preferencesModel.aiApiKey = displayValue
				control.echoMode:	TextInput.Password
				width:				connectionId.width
				fillWidth:			true
				fieldHeight:		25 * jaspTheme.uiScale
				showEyeInside:		true
				KeyNavigation.tab:	aiModelInput
			}

			TextField
			{
				id:					aiModelInput
				label:				qsTr("Model:")
				value:				preferencesModel.aiModel
				width:				connectionId.width
				fillWidth:			true
				fieldHeight:		25 * jaspTheme.uiScale
				onEditingFinished:	preferencesModel.aiModel = displayValue
				KeyNavigation.tab:	personaTabBar
			}
		}

		Item
		{
			width:			parent.width
			height:			testButton.height + testResultLabel.height + jaspTheme.generalAnchorMargin

			Button
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
		title:		qsTr("Personas")
		visible:	preferencesModel.aiEnabled

		TabView
		{
			id:					personaTabBar
			name:				"personas"
			source:				[{model: preferencesModel.aiPersonaModel, "label": "personaDisplayName", "value": "personaName" }]
			addItemManually:	true
			tabButtonWidth:		140 * jaspTheme.uiScale
			addTooltip:			qsTr("Add Persona")
			removeTooltip:		qsTr("Remove Persona")
			backgroundColor:	jaspTheme.fileMenuColorBackground

			onAddItem:			preferencesModel.aiPersonaModel.addPersona()
			onRemoveItem:		(index) => preferencesModel.aiPersonaModel.removePersona(index)
			onKeyValueChanged:	(index, value) => { if (!isDefaultPersona(index)) setModelData(index, value, nameRole) }

			// Role shortcuts (used throughout)
			readonly property int nameRole:			preferencesModel.aiPersonaModel.getRole("personaName");
			readonly property int promptRole:		preferencesModel.aiPersonaModel.getRole("personaPrompt");
			readonly property int imageRole:		preferencesModel.aiPersonaModel.getRole("personaImagePath");
			readonly property int isSystemRole:		preferencesModel.aiPersonaModel.getRole("personaIsSystem");
			readonly property int toolsRole:		preferencesModel.aiPersonaModel.getRole("personaEnabledTools");
			readonly property int capabilitiesRole:	preferencesModel.aiPersonaModel.getRole("personaEnabledCapabilities");

			function getModelData(index, role)
			{
				return preferencesModel.aiPersonaModel.data(preferencesModel.aiPersonaModel.index(index, 0), role)
			}

			function setModelData(index, data, role)
			{
				preferencesModel.aiPersonaModel.setData(preferencesModel.aiPersonaModel.index(index, 0), data, role)
			}

			function isDefaultPersona(index)
			{
				return getModelData(index, isSystemRole)
			}

			function isTabRemovable(index)
			{
				return !isDefaultPersona(index)
			}

			Component.onCompleted: currentIndex = preferencesModel.aiPersonaModel.currentPersonaIndex

			Connections {
				target: preferencesModel.aiPersonaModel
				function onCurrentPersonaIndexChanged() { Qt.callLater(function() { personaTabBar.currentIndex = preferencesModel.aiPersonaModel.currentPersonaIndex }) }
			}

			rowComponent: Item
			{
				id:					personaEditorEdit
				x:					jaspTheme.contentMargin
				height:				personaEditorColumn.implicitHeight + jaspTheme.contentMargin

				readonly property bool isSystem: personaTabBar.isDefaultPersona(rowIndex)

				function getData(role)
				{
					return personaTabBar.getModelData(rowIndex, role)
				}

				function setData(data, role)
				{
					personaTabBar.setModelData(rowIndex, data, role)
				}

				Column
				{
					id:					personaEditorColumn
					spacing:			jaspTheme.columnGroupSpacing
					width:				parent.width - 2 * jaspTheme.contentMargin

				// --- Avatar ---
					QTL.RowLayout
					{
						y:									jaspTheme.contentMargin
						spacing:							20 * jaspTheme.uiScale
						Label
						{
							text:							qsTr("Avatar:")
							QTL.Layout.alignment:			Qt.AlignHCenter
						}

						Image
						{
							id:								personaImagePreview
							QTL.Layout.alignment:			Qt.AlignHCenter
							width:							40 * preferencesModel.uiScale
							height:							width
							fillMode:						Image.PreserveAspectCrop
							asynchronous:					true
							source:							getData(personaTabBar.imageRole)

							sourceSize.width:				width
							sourceSize.height:				height
						}

						Button
						{
							id:							personaImageBrowse
							text:							qsTr("Choose another image")
							enabled:						!isSystem
							QTL.Layout.alignment:			Qt.AlignHCenter
							onClicked:						personaImageFileDialog.open()
							KeyNavigation.tab:				personaPromptInput
						}


						QTD.FileDialog {
							id:								personaImageFileDialog
							title:							qsTr("Select Persona Image")
							nameFilters:					[qsTr("Images") + "(*.png *.jpg *.jpeg *.gif *.svg)"]
							onAccepted:
							{
								var path = preferencesModel.aiPersonaModel.copyImageToPersonasDir(selectedFile)
								if (path)
									setData(path, personaTabBar.imageRole)
							}
						}
					}

					// --- Persona Prompt ---
					TextArea
					{
						id:					personaPromptInput
						isBound:			false
						title:				qsTr("Persona Prompt:")
						width:				parent.width
						height:				150 * preferencesModel.uiScale
						wrapMode:			TextEdit.Wrap
						text:				getData(personaTabBar.promptRole)
						enabled:			!isSystem
						onActiveFocusChanged: if (!activeFocus) setData(text, personaTabBar.promptRole)
						applyScriptInfo:	""
						useTabAsSpaces:		false
						nextTabItem:		capSection.button
					}

					// --- Capabilities  ---
					Section
					{
						id:			capSection
						title:		qsTr("Persona Capabilities")
						columns:	1
						property var allCapabilities: preferencesModel.aiPersonaModel.capabilities()
						property var personaCapabilities: getData(personaTabBar.capabilitiesRole)

						Group
						{
							columns: 2
							preferredWidth: capSection.width

							Repeater
							{
								id: capRepeater
								model: capSection.allCapabilities.length

								CheckBox
								{
									isBound:						false
									property var capabilityData:	capSection.allCapabilities[index]
									property string capId:			capabilityData.id
									label:							capabilityData.displayName
									enabled:						capabilityData.methods.length > 0 && !isSystem
									checked:						capSection.personaCapabilities.indexOf(capId) >= 0

									onClicked:						preferencesModel.aiPersonaModel.toggleCapability(rowIndex, capId)
								}
							}
						}
					}

					// ---- Advanced (individual tools) ----

					Section
					{
						id:			toolsSection
						title:		qsTr("Advanced")
						columns:	1
						property var allTools: preferencesModel.aiPersonaModel.allKnownToolNames()
						property var personaTools: getData(personaTabBar.toolsRole)

						Group
						{
							columns: 2
							preferredWidth: toolsSection.width

							Repeater
							{
								id: toolRepeater
								model: toolsSection.allTools

								CheckBox {
									isBound:					false
									property string toolName:	modelData
									label:						preferencesModel.aiPersonaModel.toolDisplayName(modelData)
									checked:					toolsSection.personaTools.indexOf(toolName) >= 0
									enabled:					!isSystem
									onClicked:					preferencesModel.aiPersonaModel.toggleTool(rowIndex, toolName)
								}
							}
						}
					}
					// --- Actions ---
					Row
					{
						id: personaActionsRow
						spacing: jaspTheme.generalAnchorMargin

						Button {
							text:		qsTr("Set as Active")
							onClicked:	preferencesModel.aiPersonaModel.currentPersonaIndex = rowIndex
							enabled:	preferencesModel.aiPersonaModel.currentPersonaIndex !== rowIndex
						}

						Button {
							text:		qsTr("Duplicate")
							onClicked:	preferencesModel.aiPersonaModel.duplicatePersona(rowIndex)
						}

						Button {
							text:		qsTr("Delete Persona")
							visible:	!personaTabBar.isDefaultPersona(rowIndex)
							onClicked:	preferencesModel.aiPersonaModel.removePersona(rowIndex)
						}

						Button {
							text:		qsTr("Reset to Default")
							visible:	false  // system personas can no longer be edited
							onClicked:	preferencesModel.aiPersonaModel.resetSystemPersona(rowIndex)
						}
					}
				}
			}
		}
	}

	PrefsGroupRect
	{
		title:				qsTr("Annotation")
		visible:			preferencesModel.aiEnabled

		CheckBox
		{
			id:					annotationUseCustom
			label:				qsTr("Use custom annotation prompt")
			checked:			preferencesModel.aiAnnotationUseCustom
			onCheckedChanged:	preferencesModel.aiAnnotationUseCustom = checked
			toolTip:			qsTr("When enabled, the custom prompt below is used when clicking the Annotate Analysis button instead of the default.")
		}

		TextArea
		{
			id:				aiAnnotationPromptInput
			title:			qsTr("Annotation Prompt:")
			height:			80 * preferencesModel.uiScale
			text:			preferencesModel.aiAnnotationPrompt
			isBound:		false
			wrapMode:		TextEdit.Wrap
			enabled:		annotationUseCustom.checked
			onActiveFocusChanged: if (!activeFocus) preferencesModel.aiAnnotationPrompt = text
			applyScriptInfo:""
			useTabAsSpaces:	false
			nextTabItem:	mcpEnabled

		}
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

	Section
	{
		id:			advancedSec
		title:		qsTr("Advanced")
		visible:	preferencesModel.aiEnabled
		columns:	1

		PrefsGroupRect
		{
			title:				qsTr("Additional Parameters")

			CheckBox
			{
				id:					completeSchemaCheck
				label:				qsTr("Include full tool schemas in request")
				checked:			preferencesModel.aiUseCompleteSchema
				onCheckedChanged:	preferencesModel.aiUseCompleteSchema = checked
				toolTip:			qsTr(
					"When enabled, each tool in the API request includes its full "
					+ "parameter schema (with JSON such as like integer/boolean). "
					+ "This helps models that struggle with type-safety in tool "
					+ "calls (e.g., Qwen). Uses more tokens. Leave unticked for DeepSeek."
				)
			}

			TextArea
			{
				id:				aiCommonSystemPromptInput
				title:			qsTr("Common System Prompt:")
				height:			120 * preferencesModel.uiScale
				text:			preferencesModel.aiCommonSystemPrompt
				isBound:		false
				wrapMode:		TextEdit.Wrap
				placeholderText: qsTr("Common system prompt shared across all personas…")
				onActiveFocusChanged: if (!activeFocus) preferencesModel.aiCommonSystemPrompt = text
				applyScriptInfo:""
				useTabAsSpaces:	false
				nextTabItem:	chatLimitCheck
			}

			CheckBox
			{
				id:					chatLimitCheck
				label:				qsTr("Single chat token limit:")
				childrenOnSameRow:	true
				checked:			preferencesModel.aiChatLimitActive
				onCheckedChanged:	preferencesModel.aiChatLimitActive = checked

				IntegerField
				{
					value:			preferencesModel.aiChatLimit
					onValueChanged: preferencesModel.aiChatLimit = value
					enabled:		chatLimitCheck.checked
					fieldWidth:		100 * preferencesModel.uiScale
					toolTip:		qsTr("~4 characters ≈ 1 token")
				}
			}

			Label
			{
				text:			qsTr("Paste a JSON object with extra parameters to include in every API request.\nExamples: { \"max_tokens\": 4096, \"thinking\": { \"type\": \"enabled\" } }\nFields \"model\", \"stream\", \"messages\", \"tools\", and \"text\" are protected and will be ignored.")
				wrapMode:		Text.WordWrap
				width:			parent.width
			}

			TextArea
			{
				id:				aiExtraParamsInput
				text:			preferencesModel.aiExtraParams
				height:			120 * preferencesModel.uiScale
				isBound:		false
				wrapMode:		TextEdit.Wrap
				placeholderText: qsTr("Common system prompt shared across all personas…")
				onActiveFocusChanged: if (!activeFocus) preferencesModel.aiExtraParams = text
				applyScriptInfo:""
				useTabAsSpaces:	false
				nextTabItem:	aiMessageExtraInput
				}
			}

			PrefsGroupRect
			{
				title:				qsTr("Per-Message Extra Fields")

			Label
			{
				text:			qsTr(
					"Paste a JSON object to merge into every message of the API request.\n"
					+ "Use this for per-message features like explicit caching: { \"cache_control\": { \"type\": \"ephemeral\" } }\n"
					+ "Fields \"role\", \"content\", and \"text\" are protected and will be ignored."
				)
				wrapMode:		Text.WordWrap
				width:			parent.width
			}


			TextArea
			{
				id:				aiMessageExtraInput
				text:			preferencesModel.aiMessageExtra
				height:			120 * preferencesModel.uiScale
				isBound:		false
				wrapMode:		TextEdit.Wrap
				placeholderText: qsTr("{ \"cache_control\": { \"type\": \"ephemeral\" } }")
				onActiveFocusChanged: if (!activeFocus) preferencesModel.aiMessageExtra = text
				applyScriptInfo:""
				useTabAsSpaces:	false
				nextTabItem:	mcpEnabled
			}
		}
	}

	PrefsGroupRect
	{
		title:				qsTr("AI Service")

		Button
		{
			visible:		preferencesModel.aiEnabled
			text:			qsTr("Reset all AI settings to defaults")
			toolTip:		qsTr("Restore endpoint, model, system prompt, and all other AI settings to their original defaults.")
			onClicked:		preferencesModel.resetAiDefaults()
		}

		Button
		{
			id:				aiEnableBtn
			text:			preferencesModel.aiEnabled ? qsTr("Disable") : qsTr("Enable")
			toolTip:		qsTr("Toggle AI functionality. A confirmation dialog will appear when enabling.")
			KeyNavigation.tab:	aiEndpointInput

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
	Item { height: 3; width: 3} // Add some space at the bottom.
}
