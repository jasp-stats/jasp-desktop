#include "odsxmlcontentshandler.h"

#include "log.h"

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
const QString XmlContentsHandler::_attRowRepeatCount("table:number-rows-repeated");

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
 , _lastNotEmptyColumn(-1)
 , _tableRead(false)
 , _lastType(odsType_unknown)
 , _colRepeat(1)
 , _rowRepeat(1)

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
		//Log::log() << "XmlContentsHandler::startElement. docDepth: " << _docDepth << ", localName: " << localName << ", qName: " << qName << std::endl;
		
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
			{
				_docDepth = table_row;
				_rowRepeat = _findRowRepeat(atts);
			}
			break;
		case table_row:
			if (localName == _nameTableCell)
			{
				_docDepth = table_cell;
				// Arrived at a cell.

				// Get it's type and value.
				_setLastTypeGetValue(_currentCell, atts);
				
				// Find column span for this cell.
				_colRepeat = _findColRepeat(atts);
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
		//Log::log() << "XmlContentsHandler::endElement. docDepth: " << _docDepth << ", localName: " << localName.toStdString() << ", qName: " << qName.toStdString() << std::endl;
		
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
				if (_row > 0 && _lastNotEmptyColumn > -1)
				{
					// Repeat the last row
					for (int i = 1; i < _rowRepeat; i++)
					{
						for (int j = 0; j < _dataSet->columnCount(); j++)
						{
							(*_dataSet)[j].setValue(_row, (*_dataSet)[j].getCell(_row - 1).valueAsString());
						}
						_row++;
					}
				}
				_row++;
				// Starting next column.
				_column = 0;
				_lastNotEmptyColumn = -1;
				_currentCell.clear();
				_colRepeat = 1;
				_rowRepeat = 1;
			}
			break;
		case table_cell:
			if (localName == _nameTableCell)
			{
				if (!_currentCell.isEmpty())
				{
					if (_row == 0)
					{
						// Deals with header
						// First add columns that had no name
						for (int i = _lastNotEmptyColumn + 1; i < _column; i++)
						{
							// Set some names for columns that have no header.
							stringstream ss;
							ss << "_col" << (i + 1);
							_dataSet->createColumn(ss.str());
						}
						// Create the column with the current cell name
						_dataSet->createColumn(_currentCell.toStdString());
						// Repeat create column if necessary
						for (int i = 1; i < _colRepeat; i++)
						{
							stringstream ss;
							ss << "_col" << (_column + i + 1);
							_dataSet->createColumn(ss.str());
						}
					}
					else
					{
						for (int i = _lastNotEmptyColumn + 1; i < _column; i++)
						{
							// Set empty values
							_dataSet->getOrCreate(i).setValue(_row - 1, string());
						}
						for (int i = 0; i < _colRepeat; i++)
							_dataSet->getOrCreate(_column + i).setValue(_row - 1, _currentCell.toStdString());
					}
					_lastNotEmptyColumn = _column + _colRepeat - 1;
				}
				
				_docDepth = table_row;
				_column += _colRepeat;
				_colRepeat = 1;
				_currentCell.clear();
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
		if ((_docDepth == text) && (ch.isEmpty() == false) && _currentCell.isEmpty())
		{
			//Log::log() << "Characters: " << ch << std::endl;
			_currentCell = ch;
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
	_lastNotEmptyColumn = -1;
	_tableRead = false;
	_lastType = odsType_unknown;
	_colRepeat = 1;
	_rowRepeat = 1;
	
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
int XmlContentsHandler::_findColRepeat(const QXmlAttributes &atts, int defaultValue)
{
	int result = 0;
	bool okay = false;
	result = atts.value(_attCellRepeatCount).toInt(&okay);
	return (okay) ? result : defaultValue;
}

int XmlContentsHandler::_findRowRepeat(const QXmlAttributes &atts, int defaultValue)
{
	int result = 0;
	bool okay = false;
	result = atts.value(_attRowRepeatCount).toInt(&okay);
	return (okay) ? result : defaultValue;
}
