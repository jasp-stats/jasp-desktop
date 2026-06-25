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

	// ──────────────────────────────────────────────
	// 1. Provider & Model
	// ──────────────────────────────────────────────
	PrefsGroupRect
	{
		title:				qsTr("Provider & Model")
		visible:			preferencesModel.aiEnabled

		// ── a) Provider / Model dropdowns ──
		Row
		{
			spacing:		jaspTheme.generalAnchorMargin

			DropDown
			{
				id:				providersDropdown
				label:			qsTr("Provider:")
				values:			aiConfigModel.providerValues
				currentIndex:	aiConfigModel.currentProviderIndex
				onActivated:	function(index) { aiConfigModel.currentProviderIndex = index; aiBridge.clearChat() }
				focus:			true
			}

			DropDown
			{
				label:			qsTr("Model:")
				values:			aiConfigModel.modelValues
				currentIndex:	aiConfigModel.currentModelIndex
				onActivated:	function(index) { aiConfigModel.currentModelIndex = index; aiBridge.clearChat() }
			}
		}

		// ── Warning banner ──
		Rectangle
		{
			visible:			aiConfigModel.currentWarning !== ""
			width:				parent.width
			height:				warningLabel.implicitHeight + jaspTheme.contentMargin
			color:				jaspTheme.redLighter
			border.color:		jaspTheme.redDarker
			border.width:		1
			radius:				4 * jaspTheme.uiScale

			Text
			{
				id:				warningLabel
				text:			aiConfigModel.currentWarning
				font:			jaspTheme.font
				color:			jaspTheme.redDarker
				wrapMode:		Text.WordWrap
				anchors
				{
					left:		parent.left
					right:		parent.right
					top:		parent.top
					margins:	jaspTheme.contentMargin / 2
				}
			}
		}

		// ── b) Connection ──
		PrefsGroupRect
		{
			title:				qsTr("Connection")

			Group
			{
				id:			connectionGroup
				columns:	1
				width:		parent.width

				TextField
				{
					id:					aiEndpointInput
					label:				qsTr("Endpoint URL:")
					value:				aiConfigModel.currentEndpoint
					onEditingFinished:	aiConfigModel.currentEndpoint = displayValue
					width:				connectionGroup.width
					fillWidth:			true
					fieldHeight:		25 * jaspTheme.uiScale
				}

				TextField
				{
					label:				qsTr("API Key:")
					value:				aiConfigModel.currentApiKey
					onEditingFinished:	aiConfigModel.currentApiKey = displayValue
					control.echoMode:	TextInput.Password
					showEyeInside:		true
					width:				connectionGroup.width
					fillWidth:			true
					fieldHeight:		25 * jaspTheme.uiScale
				}

				TextField
				{
					id:					aiModelInput
					label:				qsTr("Model:")
					value:				aiConfigModel.currentModel
					onEditingFinished:	aiConfigModel.currentModel = displayValue
					width:				connectionGroup.width
					fillWidth:			true
					fieldHeight:		25 * jaspTheme.uiScale
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

		// ── c) Advanced ──
		Section
		{
			title:		qsTr("Advanced")
			columns:	1

			TextArea
			{
				id:					aiSystemPromptPostfixInput
				title:				qsTr("System Prompt Postfix:")
				height:				80 * preferencesModel.uiScale
				text:				aiConfigModel.currentSystemPromptPostfix
				isBound:			false
				wrapMode:			TextEdit.Wrap
				onActiveFocusChanged:	if (!activeFocus) aiConfigModel.currentSystemPromptPostfix = text
				applyScriptInfo:	""
				useTabAsSpaces:		false
				nextTabItem:		completeSchemaCheck
			}

			CheckBox
			{
				id:					completeSchemaCheck
				label:				qsTr("Include full tool schemas in request")
				checked:			aiConfigModel.currentUseCompleteSchema
				onClicked:			aiConfigModel.currentUseCompleteSchema = checked
				toolTip:			qsTr(
					"When enabled, each tool in the API request includes its full "
					+ "parameter schema (with JSON such as like integer/boolean). "
					+ "This helps models that struggle with type-safety in tool "
					+ "calls (e.g., Qwen). Uses more tokens. Leave unticked for DeepSeek."
				)
			}

			Label
			{
				text:			qsTr("Paste a JSON object with extra parameters to include in every API request.\nExamples: { \"max_tokens\": 4096, \"thinking\": { \"type\": \"enabled\" } }\nFields \"model\", \"stream\", \"messages\", \"tools\", and \"text\" are protected and will be ignored.")
				wrapMode:		Text.WordWrap
				width:			parent.width
			}

			TextArea
			{
				id:					aiExtraParamsInput
				text:				aiConfigModel.currentExtraParams
				height:				100 * preferencesModel.uiScale
				isBound:			false
				wrapMode:			TextEdit.Wrap
				onActiveFocusChanged:	if (!activeFocus) aiConfigModel.currentExtraParams = text
				applyScriptInfo:	""
				useTabAsSpaces:		false
				nextTabItem:		chatLimitCheck

			}

			CheckBox
			{
				id:					chatLimitCheck
				label:				qsTr("Single chat token limit:")
				childrenOnSameRow:	true
				checked:			aiConfigModel.currentChatLimitActive
				onClicked:			aiConfigModel.currentChatLimitActive = checked

				IntegerField
				{
					value:			aiConfigModel.currentChatLimit
					onEditingFinished:	aiConfigModel.currentChatLimit = displayValue
					enabled:		chatLimitCheck.checked
					fieldWidth:		100 * preferencesModel.uiScale
					toolTip:		qsTr("~4 characters ≈ 1 token")
				}
			}

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
				id:					aiMessageExtraInput
				text:				aiConfigModel.currentMessageExtra
				height:				100 * preferencesModel.uiScale
				isBound:			false
				wrapMode:			TextEdit.Wrap
				placeholderText:	qsTr("{ \"cache_control\": { \"type\": \"ephemeral\" } }")
				onActiveFocusChanged:	if (!activeFocus) aiConfigModel.currentMessageExtra = text
				applyScriptInfo:	""
				useTabAsSpaces:		false
				nextTabItem:		personasGroup

			}
		}

		// ── Reset ──
		Row
		{
			spacing:		jaspTheme.generalAnchorMargin

			Button
			{
				text:		qsTr("Reset Model")
				toolTip:	qsTr("Reset the currently selected model's extra params, system prompt postfix, and advanced checkboxes back to their shipped defaults.")
				onClicked:	aiConfigModel.resetCurrentModelToDefaults()
			}
		}
	}

	// ──────────────────────────────────────────────
	// 2. Personas
	// ──────────────────────────────────────────────
	PrefsGroupRect
	{
		id:			personasGroup
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
			backgroundColor:	jaspTheme.uiBackground

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

			Connections
			{
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
							visible:						!isSystem
							QTL.Layout.alignment:			Qt.AlignHCenter
							onClicked:						personaImageFileDialog.open()
							KeyNavigation.tab:				personaPromptInput
							focus:							!isSystem
						}


						QTD.FileDialog {
							id:								personaImageFileDialog
							title:							qsTr("Select Persona Image")
							nameFilters:					[qsTr("Images") + "(*.png *.jpg *.jpeg *.gif *.svg)"]
							onAccepted:						setData(selectedFile, personaTabBar.imageRole)
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
							focus:		isSystem
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
					}
				}
			}
		}

		// ── Common System Prompt ──
		CheckBox
		{
			id:					commonSystemPromptUseCustom
			label:				qsTr("Use custom system prompt")
			checked:			preferencesModel.aiCommonSystemPromptUseCustom
			onCheckedChanged:	preferencesModel.aiCommonSystemPromptUseCustom = checked
			toolTip:			qsTr("When enabled, the custom prompt below is used as the system prompt instead of the default.")
		}

		TextArea
		{
			id:					aiCommonSystemPromptInput
			title:				qsTr("Common System Prompt:")
			height:				120 * preferencesModel.uiScale
			text:				preferencesModel.aiCommonSystemPrompt
			isBound:			false
			wrapMode:			TextEdit.Wrap
			enabled:			commonSystemPromptUseCustom.checked
			placeholderText:		qsTr("Common system prompt shared across all personas…")
			onActiveFocusChanged:	if (!activeFocus) preferencesModel.aiCommonSystemPrompt = text
			applyScriptInfo:	""
			useTabAsSpaces:		false
			nextTabItem:		annotationUseCustom

		}
	}

	// ──────────────────────────────────────────────
	// 3. Annotation
	// ──────────────────────────────────────────────
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
			nextTabItem:	userAvatarFileButton

		}
	}

	// ──────────────────────────────────────────────
	// 4. Chat Appearance
	// ──────────────────────────────────────────────
	PrefsGroupRect
	{
		title:		qsTr("Chat Appearance")
		visible:	preferencesModel.aiEnabled

		QTL.RowLayout
		{
			spacing: 12 * jaspTheme.uiScale

			Label
			{
				text:					qsTr("My icon:")
				QTL.Layout.alignment:	Qt.AlignVCenter
			}

			Image
			{
				id:					userAvatarPreview
				QTL.Layout.alignment:	Qt.AlignVCenter
				width:				28 * preferencesModel.uiScale
				height:				width
				fillMode:			Image.PreserveAspectCrop
				asynchronous:		true
				source:				preferencesModel.aiPersonaModel.userAvatar
				sourceSize.width:	width
				sourceSize.height:	height
			}

			Button
			{
				id:						userAvatarFileButton
				text:					qsTr("Choose image…")
				QTL.Layout.alignment:	Qt.AlignVCenter
				onClicked:				userAvatarFileDialog.open()
				KeyNavigation.tab:		mcpEnabled
			}

			QTD.FileDialog
			{
				id: userAvatarFileDialog
				title: qsTr("Select Your Avatar")
				nameFilters: [qsTr("Images") + "(*.png *.jpg *.jpeg *.gif *.svg)"]
				currentFolder: preferencesModel.aiPersonaModel.shippedPersonaImagesDir()
				onAccepted: preferencesModel.aiPersonaModel.userAvatar = selectedFile;
			}
		}
	}

	// ──────────────────────────────────────────────
	// 5. MCP
	// ──────────────────────────────────────────────
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

	// ──────────────────────────────────────────────
	// 6. AI Service
	// ──────────────────────────────────────────────
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
			KeyNavigation.tab:	providersDropdown

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
