#ifndef ODSXMLCONTENTSHANDLER_H
#define ODSXMLCONTENTSHANDLER_H

#include <vector>

#include "odsxmlhandler.h"
#include "odstypes.h"

namespace ods
{

class ODSXmlContentsHandler : public XmlHandler
{
	// Depth in XML document.
	typedef enum e_docDepth
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
		text			// Only for string cells.
	} DocDepth;

public:
	ODSXmlContentsHandler(ODSImportDataSet *dta);

	/**
	 * @brief startElement Called on the start of an element.
	 * @param namespaceURI - the URI.
	 * @param localName - local name (name without prefix).
	 * @param qName - Qualified name.
	 * @param atts- Attributes.
	 * @return true on no error found.
	 *
	 * Called when a <tag ...> construction found.
	 *
	 */
	virtual bool startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &atts);

	/**
	 * @brief endElement Called on the end of an element.
	 * @param namespaceURI - the URI.
	 * @param localName - local name (name without prefix).
	 * @param qName - Qualified name.
	 * @param atts- Attributes.
	 * @return true on no error found.
	 *
	 * Called when a </tag> construction found.
	 *
	 */
	virtual bool endElement(const QString &namespaceURI, const QString &localName, const QString &qName);

	/**
	 * @brief characters Called when char data found.
	 * @param ch The found data.
	 * @return true on no error.
	 */
	virtual bool characters(const QString &ch);

	/**
	 * @brief resetDocument Reset level, row and column, clears data.
	 */
	void resetDocument();

private:
	DocDepth 		_docDepth			= DocDepth::not_in_doc;		///< Current depth of document.
	size_t			_row				= 0;						///< Current row in document/table.
	int				_column				= 0,						///< Current column in document/table.
					_lastNotEmptyColumn	= -1;
	bool			_tableRead			= false;					///< True if first table read.
	XmlDatatype		_lastType			= odsType_unknown;			///< The last type we found in a opening tag.
	int				_colRepeat			= 1,						///< Number cells this XML element spans.
					_rowRepeat			= 1;
	QString			_currentCell,
					_currentComment;

	// Names we search for.
	static const QString _nameBody;
	static const QString _nameTable;
	static const QString _nameTableRow;
	static const QString _nameDocContent;
	static const QString _nameSpreadsheet;
	static const QString _nameAnnotation;
	static const QString _nameTableCell;
	static const QString _nameText;

	// Attribute names we search for.
	static const QString _attValue;
	static const QString _attValueType;
	static const QString _attDateValue;
	static const QString _attTimeValue;
	static const QString _attBoolValue;
	static const QString _attCellRepeatCount;
	static const QString _attRowRepeatCount;

	// Values of the attribute attValueType.
	static const QString _typeCurrency;
	static const QString _typePercent;
	static const QString _typeBoolean;
	static const QString _typeString;
	static const QString _typeFloat;
	static const QString _typeDate;
	static const QString _typeTime;
	
	// Excel sometimes exports too many "repeat columns/row" elements, only to make sure that it looks the same as in excel.
	// In the sense of looking the same as the entire editable table in excel...
	// It then wants you to repeat empty cells that many times.
	// This is of course not very sensible so instead we detect that and ignore such cells.
	// To do this we need to know the maximum size of an excelspreadsheet and it is:
	const int				_excelMaxRows = 1048576, 
							_excelMaxCols = 16384;


	/**
	 * @brief XmlContentsHandler::setLastType Sets the lastType value, and gets value
	 * @param QValue value OUTPIT value found.
	 * @param QXmlAttributes atts Attriutes to find.
	 * @return value of lastType;
	 */
	XmlDatatype _setLastTypeGetValue(QString &value, const QXmlAttributes &atts);

	/**
	 * @brief _findColRepeat/_findRowRepeat Finds the column/row repeat from attributes.
	 * @param atts The attribuyes to search.
	 * @param defaultValue The value to return if not found.
	 * @return The found value or default.
	 */
	static int _findColRepeat(const QXmlAttributes &atts, int defaultValue = 1);
	static int _findRowRepeat(const QXmlAttributes &atts, int defaultValue = 1);

};

} // end namepsace

#endif // ODSXMLCONTENTSHANDLER_H
