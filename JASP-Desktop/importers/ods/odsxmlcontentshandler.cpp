#include "odsxmlcontentshandler.h"


using namespace std;
using namespace ods;

const QString XmlContentsHandler::nameDocContent("document-content");
const QString XmlContentsHandler::nameBody("body");
const QString XmlContentsHandler::nameSpreadsheet("spreadsheet");
const QString XmlContentsHandler::nameTable("table");
const QString XmlContentsHandler::nameTableRow("table-row");
const QString XmlContentsHandler::nameTableCell("table-cell");
const QString XmlContentsHandler::nameText("p");


XmlContentsHandler::XmlContentsHandler(Data *dta)
 : XmlHandler(dta)
 , _docDepth(not_in_doc)
 , _row(0)
 , _column(0)
 , _tableRead(false)
 , _lastType(Data::odsType_unknown)
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
	static const QString attCellRepeatCount("table:number-columns-repeated");

	if (_tableRead == false)
	{
		// Where were we?
		switch(_docDepth)
		{
		case not_in_doc:
			if (localName == nameDocContent)
				_docDepth = document_content;
			break;
		case document_content:
			if (localName == nameBody)
				_docDepth = body;
			break;
		case body:
			if (localName == nameSpreadsheet)
				_docDepth = spreadsheet;
			break;
		case spreadsheet:
			if (localName == nameTable)
				_docDepth = table;
			break;
		case table:
			if (localName == nameTableRow)
				_docDepth = table_row;
			break;
		case table_row:
			if (localName == nameTableCell)
			{
				_docDepth = table_cell;
				// Arrived at a cell.

				Data::Sheet & sheet = _odsData->sheet();
				// Get it's type and value.
				QString value;
				_setLastType(value, atts);


				// Find the colum span, and see if we have space for it..
				// Headers.
				_colSpan = 1;	// Default
				if ( (_row == 0) && (_lastType != Data::odsType_unknown) )
					sheet.createSpace(_column);
				else
				{
					{
					bool okay = false;
					int cols = atts.value(attCellRepeatCount).toInt(&okay);
					if (okay)
						_colSpan = cols;
					}

					// Is cell repeated across all the cols in this row, and blank?
					if (  (_colSpan >= sheet.numColumns())
						&&(_column == 0)
						&&(value.size() == 0)
						&&(_lastType == Data::odsType_unknown) )
						break;

					if ((_column + _colSpan) > sheet.numColumns())
						_colSpan = sheet.numColumns() - _column;
				}

				// Are we to the left of the
				// rightmost first cell?
				if (   ((_column + _colSpan) <= sheet.numColumns() )	// Enough space?
					&& (_lastType != Data::odsType_string) )			// No delayed?
				{
					if (_colSpan > 0)
					{
						for (; _colSpan > 0; _colSpan--)
							sheet[_column++].insert(_row, _lastType, value);
						_column--;
					}
				}
			}
			break;

		case table_cell:
			if (localName == nameText)
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
			if (localName == nameDocContent)
				_docDepth = not_in_doc;
			break;
		case body:
			if (localName == nameBody)
				_docDepth = document_content;
			break;
		case spreadsheet:
			if (localName == nameSpreadsheet)
				_docDepth = body;
			break;
		case table:
			if (localName == nameTable)
			{
				_docDepth = spreadsheet;
				_tableRead = true;
			}
			break;
		case table_row:
			if (localName == nameTableRow)
			{
				_docDepth = table;
				_row++;
				// Starting next column.
				_column = 0;
			}
			break;
		case table_cell:
			if (localName == nameTableCell)
			{
				_docDepth = table_row;
				_column++;
			}
			break;

		case text:
			if (localName == nameText)
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
		// Have we deferred the value from the header?
		if ((_docDepth == text) && (_lastType == Data::odsType_string))
		{
			if (_colSpan > 0)
			{
				for (; _colSpan > 0; _colSpan--)
					_odsData->sheet()[_column++].insert(_row, _lastType, ch);
				_column--;
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
	_odsData->sheet().clear();
}

/**
 * @brief XmlContentsHandler::setLastType Sets the lastType value, and gets value
 * @param QValue value OUTPIT value found.
 * @param QXmlAttributes atts Attriutes to find.
 * @return value of lastType;
 */
Data::XmlDatatype XmlContentsHandler::_setLastType(QString &value, const QXmlAttributes &atts)
{
	static const QString attValueType("office:value-type");
	static const QString attValue("office:value");
	static const QString attDateValue("office:date-value");
	static const QString attTimeValue("office:time-value");
	static const QString attBoolValue("office:boolean-value");

	static const QString typeFloat("float");
	static const QString typeCurrency("currency");
	static const QString typePercent("percentage");
	static const QString typeBoolean("boolean");
	static const QString typeString("string");
	static const QString typeDate("date");
	static const QString typeTime("time");

	_lastType = Data::odsType_unknown;
	QString fromfile = atts.value(attValueType);

	if (fromfile == typeFloat)
	{
		_lastType = Data::odsType_float;
		value = atts.value(attValue);
	}
	else if (fromfile == typeCurrency)
	{
		_lastType = Data::odsType_currency;
		value = atts.value(attValue);
	}
	else if (fromfile == typePercent)
	{
		_lastType = Data::odsType_percent;
		value = atts.value(attValue);
	}
	else if (fromfile == typeBoolean)
	{
		_lastType = Data::odsType_boolean;
		value = atts.value(attBoolValue);
	}
	else if (fromfile == typeDate)
	{
		_lastType = Data::odsType_date;
		value = atts.value(attDateValue);
	}
	else if (fromfile == typeTime)
	{
		_lastType = Data::odsType_time;
		value = atts.value(attTimeValue);
	}
	else if (fromfile == typeString)
		_lastType = Data::odsType_string;

	return _lastType;
}
