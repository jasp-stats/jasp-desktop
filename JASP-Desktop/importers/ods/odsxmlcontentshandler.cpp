#include "odsxmlcontentshandler.h"

using namespace std;
using namespace ods;

const QString XmlContentsHandler::_nameDocContent("document-content");
const QString XmlContentsHandler::_nameBody("body");
const QString XmlContentsHandler::_nameSpreadsheet("spreadsheet");
const QString XmlContentsHandler::_nameTable("table");
const QString XmlContentsHandler::_nameTableRow("table-row");
const QString XmlContentsHandler::_nameTableCell("table-cell");
const QString XmlContentsHandler::_nameText("p");

const QString XmlContentsHandler::_attValueType("office:value-type");
const QString XmlContentsHandler::_attValue("office:value");
const QString XmlContentsHandler::_attDateValue("office:date-value");
const QString XmlContentsHandler::_attTimeValue("office:time-value");
const QString XmlContentsHandler::_attBoolValue("office:boolean-value");
const QString XmlContentsHandler::_attCellRepeatCount("table:number-columns-repeated");

const QString XmlContentsHandler::_typeFloat("float");
const QString XmlContentsHandler::_typeCurrency("currency");
const QString XmlContentsHandler::_typePercent("percentage");
const QString XmlContentsHandler::_typeBoolean("boolean");
const QString XmlContentsHandler::_typeString("string");
const QString XmlContentsHandler::_typeDate("date");
const QString XmlContentsHandler::_typeTime("time");


XmlContentsHandler::XmlContentsHandler(ODSImportDataSet *dta)
 : XmlHandler(dta)
 , _docDepth(not_in_doc)
 , _row(0)
 , _column(0)
 , _tableRead(false)
 , _lastType(odsType_unknown)
 , _colSpan(0)

{

}

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
bool XmlContentsHandler::startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &atts)
{
	if (_tableRead == false)
	{
		// Where were we?
		switch(_docDepth)
		{
		case not_in_doc:
			if (localName == _nameDocContent)
				_docDepth = document_content;
			break;
		case document_content:
			if (localName == _nameBody)
				_docDepth = body;
			break;
		case body:
			if (localName == _nameSpreadsheet)
				_docDepth = spreadsheet;
			break;
		case spreadsheet:
			if (localName == _nameTable)
				_docDepth = table;
			break;
		case table:
			if (localName == _nameTableRow)
				_docDepth = table_row;
			break;
		case table_row:
			if (localName == _nameTableCell)
			{
				_docDepth = table_cell;
				// Arrived at a cell.

				// Get it's type and value.
				QString value;
				_setLastTypeGetValue(value, atts);

				// Deal with headers / create the column.
				if ((_row == 0) && (_lastType != odsType_unknown))
					_dataSet->createSpace(_column);

				// Find column span for this cell.
				_colSpan = _findColspan(atts);

				//  Blank row? - Skip it.
				if ((_row != 0) && (_colSpan > _dataSet->columnCount()) && (_column == 0))
						break;

				// Is the end column off the end of the active area?
				int effectiveColSpan = _colSpan;
				if ((_column + _colSpan) >= _dataSet->columnCount())
					effectiveColSpan = _dataSet->columnCount() - _column;
				// Give up
				if (effectiveColSpan == 0)
					break;

				// Create a blank cells, one for each spanned column.
				if (effectiveColSpan > 0)
				{
					for (int column = _column; column < (effectiveColSpan + _column); ++column)
						(*_dataSet)[column].createSpace(_row);
				}

				// Data cells (i.e. not first row), and we actually have a value to insert?
				if ((_row > 0) && (value.isEmpty() == false))
					(*_dataSet)[_column].setValue(_row, _lastType, value);
			}
			break;

		case table_cell:
			if (localName == _nameText)
				_docDepth = text;
			break;

		case text:
			break;
		}
	} // if ! table read.

	return true;
}

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
bool XmlContentsHandler::endElement(const QString &namespaceURI, const QString &localName, const QString &qName)
{
	if (_tableRead == false)
	{
		switch(_docDepth)
		{
		case not_in_doc:
			break;
		case document_content:
			if (localName == _nameDocContent)
				_docDepth = not_in_doc;
			break;
		case body:
			if (localName == _nameBody)
				_docDepth = document_content;
			break;
		case spreadsheet:
			if (localName == _nameSpreadsheet)
				_docDepth = body;
			break;
		case table:
			if (localName == _nameTable)
			{
				_docDepth = spreadsheet;
				_tableRead = true;
			}
			break;
		case table_row:
			if (localName == _nameTableRow)
			{
				_docDepth = table;
				_row++;
				// Starting next column.
				_column = 0;
				_colSpan = 0;
			}
			break;
		case table_cell:
			if (localName == _nameTableCell)
			{
				_docDepth = table_row;
				_column += _colSpan;
			}
			break;

		case text:
			if (localName == _nameText)
				_docDepth = table_cell;
			break;
		}
	}

	return true;
}


/**
 * @brief characters Called when char data found.
 * @param ch The found data.
 * @return true on no error.
 */
bool XmlContentsHandler::characters(const QString &ch)
{

	if (_tableRead == false)
	{
		// Always save the string.
		if ((_docDepth == text) && (ch.isEmpty() == false))
		{
			for (int column = _column; column < (_colSpan + _column); ++column)
			{
				if ((*_dataSet)[column].getCell(_row).xmlType() == odsType_unknown)
					(*_dataSet)[column].setValue(_row, odsType_string, ch);
				else
					(*_dataSet)[column].setValue(_row, ch);
			}
		}
	}
	return true;
}


/**
 * @brief resetDocument Reset level, row and column, clears data.
 */
void XmlContentsHandler::resetDocument()
{
	_docDepth = not_in_doc;
	_row = 0;
	_column = 0;
	_dataSet->clear();
}

/**
 * @brief XmlContentsHandler::setLastType Sets the lastType value, and gets value
 * @param QValue value OUTPIT value found.
 * @param QXmlAttributes atts Attriutes to find.
 * @return value of lastType;
 */
XmlDatatype XmlContentsHandler::_setLastTypeGetValue(QString &value, const QXmlAttributes &atts)
{
	_lastType = odsType_unknown;
	QString fromfile = atts.value(_attValueType);

	if (fromfile == _typeFloat)
		_lastType = odsType_float;
	else if (fromfile == _typeCurrency)
		_lastType = odsType_currency;
	else if (fromfile == _typePercent)
		_lastType = odsType_percent;
	else if (fromfile == _typeBoolean)
		_lastType = odsType_boolean;
	else if (fromfile == _typeDate)
		_lastType = odsType_date;
	else if (fromfile == _typeTime)
		_lastType = odsType_time;
	else if (fromfile == _typeString)
		_lastType = odsType_string;

	switch(_lastType)
	{
	case odsType_float:
	case odsType_currency:
	case odsType_percent:
		value = atts.value(_attValue);
		break;
	case odsType_boolean:
		value = atts.value(_attBoolValue);
		break;
	case odsType_date:
		value = atts.value(_attDateValue);
		break;
	case odsType_time:
		value = atts.value(_attTimeValue);
		break;
	case odsType_string:
	case odsType_unknown:
		value.clear();
	}

	return _lastType;
}

/**
 * @brief _findColspan Finds the column span from attributes.
 * @param atts The attribuyes to search.
 * @param defaultValue The value to return if not found.
 * @return The found value or default.
 */
int XmlContentsHandler::_findColspan(const QXmlAttributes &atts, int defaultValue)
{
	int result = 0;
	bool okay = false;
	result = atts.value(_attCellRepeatCount).toInt(&okay);
	return (okay) ? result : defaultValue;
}
