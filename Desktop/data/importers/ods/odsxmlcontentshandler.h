#ifndef ODSXMLCONTENTSHANDLER_H
#define ODSXMLCONTENTSHANDLER_H

#include <QXmlStreamReader>
#include "odsxmlhandler.h"
#include "odstypes.h"

namespace ods
{

class ODSXmlContentsHandler : public XmlHandler
{
	enum DocDepth
	{
		not_in_doc = -1,
		document_content,
		body,
		spreadsheet,
		table,
		table_row,
		table_cell,
		annotation,
		text_annotation,
		text
	};

public:
	explicit ODSXmlContentsHandler(ODSImportDataSet *dta);

	bool 	parse(QXmlStreamReader &reader);
	void 	resetDocument();

private:
	void		processStartElement(const QXmlStreamReader &reader);
	void		processEndElement(const QXmlStreamReader &reader);
	void		processCharacters(const QString &text);

	XmlDatatype	_setLastTypeGetValue(QString &value, const QXmlStreamAttributes &atts);
	static int	_findColRepeat(const QXmlStreamAttributes &atts, int defaultValue = 1);
	static int	_findRowRepeat(const QXmlStreamAttributes &atts, int defaultValue = 1);

	DocDepth		_docDepth			= not_in_doc;
	size_t			_row				= 0;
	int				_column				= 0,
					_lastNotEmptyColumn = -1;
	bool			_tableRead			= false;
	XmlDatatype		_lastType			= odsType_unknown;
	int				_colRepeat			= 1,
					_rowRepeat			= 1;
	QString			_currentCell,
					_currentComment;
	// Names of elements
	static const QString _nameBody, _nameTable, _nameTableRow, _nameDocContent,
						_nameSpreadsheet, _nameAnnotation, _nameTableCell, _nameText;

	// Attribute names
	static const QString _attValue, _attValueType, _attDateValue, _attTimeValue,
						_attBoolValue, _attCellRepeatCount, _attRowRepeatCount;

	// Values of the attribute attValueType
	static const QString _typeCurrency, _typePercent, _typeBoolean, _typeString,
						_typeFloat, _typeDate, _typeTime;

	// We don't wants to repeat empty cells too many times.
	// This is of course not very sensible so instead we detect that and ignore such cells.
	// To do this we need to know the maximum size of an excelspreadsheet and it is:
	const int _excelMaxRows = 1048576, _excelMaxCols = 16384;
};

}

#endif // ODSXMLCONTENTSHANDLER_H
