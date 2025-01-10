import QtQuick
import JASP.Controls
import JASP

TextArea
{
	textType: JASP.TextTypeJAGSmodel
	showLineNumber: true
	RSyntaxHighlighterQuick
	{
		textDocument:		parent.textDocument
	}
}
