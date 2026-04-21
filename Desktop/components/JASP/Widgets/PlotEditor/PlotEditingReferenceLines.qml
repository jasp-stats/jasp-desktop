import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Widgets			as	JASPW
import JASP.Theme
import JASP.Controls		as	JASPC
import JASP.PlotEditor
import JASP


JASPW.JASPDataView
{
	id:				jaspDataView
	model:			plotEditorModel.references	
	
	onWidthChanged:	plotEditorModel.references.viewWidth = width
	
	rowNumberDelegate:	null
	
	itemDelegate:	Component { Loader 
	{ 
		
		property int		rowIdx:			rowIndex
		property int		columnIdx:		columnIndex
		property string		modelText:		itemText
		property var		modelData:		itemData
		property bool		modelEnabled:	itemEnabled
		
		sourceComponent: columnIndex === 0 ? typeSelector : columnIndex == 4 ? eraseButton : textView;
	}}
	
	
	editDelegate:   Component { Loader 
	{ 
			
			property int            rowIdx:                 rowIndex
			property int            columnIdx:              columnIndex
			property string         modelText:              itemText
			property var            modelData:              itemData
			//property bool			modelEnabled:			itemEnabled
		
			sourceComponent: textEdit
	}}

	Component
	{
		id:		textView
		
		Text		
		{  
			text:		modelText == "" ? "..." : modelText;
			font:		jaspTheme.font; 
			color:		enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled; 
			enabled:	modelEnabled
			visible:	enabled
			MouseArea
			{ 
				anchors.fill:	parent; 
				onClicked:		jaspDataView.view.edit(rowIdx, columnIdx)
			}
			
		//	opacity:				rowIdx < jaspDataView.view.rowCount - 1 ? 1.0 : 0.5
			
		}
	}
	
	Component
	{
		id:		textEdit
		
		TextInput
		{
			id:						editItem
			text:					modelText
			color:					enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled; 
			font:					jaspTheme.font
			focus:					true
			clip:					true
			//onTextEdited:			jaspDataView.view.model.setData(jaspDataView.view.model.index(rowIdx, columnIdx), text)
			onEditingFinished:		{saveEdit(); jaspDataView.view.forceActiveFocus(); }
			Keys.onReturnPressed:	{saveEdit(); jaspDataView.view.forceActiveFocus(); }
			Component.onCompleted:	forceActiveFocus()
			//enabled:				modelEnabled
			onActiveFocusChanged:	
			{
				if(!activeFocus)
				{
					
					text = Qt.binding(function() {return modelText;});
				}
			}
			
			function saveEdit()
			{
				jaspDataView.view.commitEdit(rowIdx, columnIdx, text);
				jaspDataView.view.clearEdit();
			}
			
			//opacity:				rowIdx < jaspDataView.view.rowCount - 1 ? 1.0 : 0.5
		}
	}
	
	ListModel 
	{
		id: typeModel
	
		ListElement {	value:  0; name:	qsTr("Point")			}
		ListElement {	value:  1; name:	qsTr("Horizontal Line")	}
		ListElement {	value:  2; name:	qsTr("Vertical Line")	}
	}
	
	Component
	{
		id:		typeSelector
		
		JASPC.DropDown
		{  
			fieldWidth:				width
			source:					typeModel
			currentIndex:			modelData
			onValueChanged:	
			{
				if(modelData != currentIndex)
					jaspDataView.view.model.setData(jaspDataView.view.model.index(rowIdx, columnIdx), currentIndex);
			}
			
			//opacity:	rowIdx < jaspDataView.view.rowCount - 1 ? 1.0 : 0.5
		}
	}
	
	Component
	{
		id:		eraseButton

		
		JASPC.RectangularButton
		{		
			text:			"X"
			onClicked:		model.setData(model.index(rowIdx, columnIdx), true);
			//visible:		rowIdx < jaspDataView.view.rowCount - 1
		}
	}

}
