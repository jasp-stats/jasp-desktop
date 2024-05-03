#include "odsxmlcontentshandler.h"
#include "odsimportcolumn.h"
#include "utilities/qutils.h"

using namespace std;
using namespace ods;

const QString ODSXmlContentsHandler::_nameDocContent("document-content");
const QString ODSXmlContentsHandler::_nameBody("body");
const QString ODSXmlContentsHandler::_nameSpreadsheet("spreadsheet");
const QString ODSXmlContentsHandler::_nameTable("table");
const QString ODSXmlContentsHandler::_nameTableRow("table-row");
const QString ODSXmlContentsHandler::_nameTableCell("table-cell");
const QString ODSXmlContentsHandler::_nameAnnotation("annotation");
const QString ODSXmlContentsHandler::_nameText("p");

const QString ODSXmlContentsHandler::_attValueType("office:value-type");
const QString ODSXmlContentsHandler::_attValue("office:value");
const QString ODSXmlContentsHandler::_attDateValue("office:date-value");
const QString ODSXmlContentsHandler::_attTimeValue("office:time-value");
const QString ODSXmlContentsHandler::_attBoolValue("office:boolean-value");
const QString ODSXmlContentsHandler::_attCellRepeatCount("table:number-columns-repeated");
const QString ODSXmlContentsHandler::_attRowRepeatCount("table:number-rows-repeated");

const QString ODSXmlContentsHandler::_typeFloat("float");
const QString ODSXmlContentsHandler::_typeCurrency("currency");
const QString ODSXmlContentsHandler::_typePercent("percentage");
const QString ODSXmlContentsHandler::_typeBoolean("boolean");
const QString ODSXmlContentsHandler::_typeString("string");
const QString ODSXmlContentsHandler::_typeDate("date");
const QString ODSXmlContentsHandler::_typeTime("time");


ODSXmlContentsHandler::ODSXmlContentsHandler(ODSImportDataSet *dta)
 : XmlHandler(dta)
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
bool ODSXmlContentsHandler::startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &atts)
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
			if (localName == _nameAnnotation)
				_docDepth = annotation;
			else if (localName == _nameText)
				_docDepth = text;
			break;
			
		case annotation:
			if (localName == _nameText)
				_docDepth = text_annotation;
			break;
		
		case text:
		case text_annotation:
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
bool ODSXmlContentsHandler::endElement(const QString &namespaceURI, const QString &localName, const QString &qName)
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
				//Repeat some rows but only do it if it *isnt* to make the data the same size as the max excel allows...
				if (_row > 0 && _lastNotEmptyColumn > -1 && _row + _rowRepeat != _excelMaxRows)
				{
					// Repeat the last row
					for (int i = 1; i < _rowRepeat; i++)
					{
						_dataSet->createSpace(++_row);
						
						for (int j = 0; j < _dataSet->columnCount(); j++)
						{
							(*_dataSet)[j].setValue(_row,	(*_dataSet)[j].getCell(_row - 1).valueAsString());
							(*_dataSet)[j].setComment(_row, (*_dataSet)[j].getCell(_row - 1).commentAsString());
						}
						
					}
				}
				_row++;
				// Starting next row.
				_column				= 0;
				_lastNotEmptyColumn = -1;
				_currentCell		.clear();
				_currentComment		.clear();
				_colRepeat			= 1;
				_rowRepeat			= 1;
			}
			break;
			
		case table_cell:
			if (localName == _nameTableCell)
			{
				if(_row == 0)
				{
					if (!_currentCell.isEmpty()) //we have some headertext
					{
						// There is some celldata and we dont have any rows yet, so create headers:
						// Deals with header
						// First add columns that had no name/data
						for (int i = _lastNotEmptyColumn+1; i < _column; i++)
							_dataSet->createColumn("");
						
						auto & col = _dataSet->createColumn(_currentCell.toStdString());
							
						if(!_currentComment.isEmpty())
							col.setTitle(fq(_currentComment));
						
						_lastNotEmptyColumn = _column;
					}
					
					_column += _colRepeat;
				}
				else 
				{
					// Set empty values
					for (int i = _lastNotEmptyColumn+1; i < _column; i++)
						_dataSet->getOrCreate(i).setValue(_row - 1, "");
					
					if((!_currentCell.isEmpty() || !_currentComment.isEmpty()) && _column + _colRepeat != _excelMaxCols)
					{
						for (int i = 0; i < _colRepeat; i++)
						{
							ODSImportColumn & col = _dataSet->getOrCreate(_column + i);
							col.setValue(_row - 1, fq(_currentCell));
							
							if(!_currentComment.isEmpty())
								col.setComment(_row - 1, fq(_currentComment));
						}
					
						_lastNotEmptyColumn = _column + _colRepeat - 1;
					}
					
					_column += _colRepeat;
				}
				
				_docDepth		=	table_row;
				_colRepeat		=	1;
				_currentCell	.	clear();
				_currentComment	.	clear();
			}
			break;
		
		case annotation:
			if (localName == _nameAnnotation)
				_docDepth = table_cell;
			break;
			
		case text_annotation:
			if (localName == _nameText)
				_docDepth = annotation;
			break;

		case text:
			if (localName == _nameText)
				_docDepth = table_cell;
			break;
		}
	}

	return true;
}

bool ODSXmlContentsHandler::characters(const QString &ch)
{

	if (_tableRead == false)
	{
		if (!ch.isEmpty())
		{
			//Log::log() << "Characters " << (_docDepth == text ? "text" : _docDepth == text_annotation ? "text_anno" : "???") << ": " << ch << std::endl;
			switch(_docDepth)
			{
			case text:
				if(_currentCell != ch)
					_currentCell.push_back(ch);
				break;
			
			case text_annotation:
				if(_currentComment.size())
					_currentComment.push_back("\t");
				_currentComment.push_back(ch);
				break;
				
			default:
				break;
			}
			
		}
	}
	return true;
}


/**
 * @brief resetDocument Reset level, row and column, clears data.
 */
void ODSXmlContentsHandler::resetDocument()
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

XmlDatatype ODSXmlContentsHandler::_setLastTypeGetValue(QString &value, const QXmlAttributes &atts)
{
	_lastType = odsType_unknown;
	QString fromfile = atts.value(_attValueType);

	if (fromfile == _typeFloat)				_lastType = odsType_float;
	else if (fromfile == _typeCurrency)		_lastType = odsType_currency;
	else if (fromfile == _typePercent)		_lastType = odsType_percent;
	else if (fromfile == _typeBoolean)		_lastType = odsType_boolean;
	else if (fromfile == _typeDate)			_lastType = odsType_date;
	else if (fromfile == _typeTime)			_lastType = odsType_time;
	else if (fromfile == _typeString)		_lastType = odsType_string;

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
 * @param atts The attributes to search.
 * @param defaultValue The value to return if not found.
 * @return The found value or default.
 */
int ODSXmlContentsHandler::_findColRepeat(const QXmlAttributes &atts, int defaultValue)
{
	int result = 0;
	bool okay = false;
	result = atts.value(_attCellRepeatCount).toInt(&okay);
	return (okay) ? result : defaultValue;
}

int ODSXmlContentsHandler::_findRowRepeat(const QXmlAttributes &atts, int defaultValue)
{
	int result = 0;
	bool okay = false;
	result = atts.value(_attRowRepeatCount).toInt(&okay);
	return (okay) ? result : defaultValue;
}
