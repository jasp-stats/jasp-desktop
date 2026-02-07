#include "odsxmlcontentshandler.h"
#include "odsimportcolumn.h"
#include "utilities/qutils.h"


const QString ods::ODSXmlContentsHandler::_nameDocContent("document-content");
const QString ods::ODSXmlContentsHandler::_nameBody("body");
const QString ods::ODSXmlContentsHandler::_nameSpreadsheet("spreadsheet");
const QString ods::ODSXmlContentsHandler::_nameTable("table");
const QString ods::ODSXmlContentsHandler::_nameTableRow("table-row");
const QString ods::ODSXmlContentsHandler::_nameTableCell("table-cell");
const QString ods::ODSXmlContentsHandler::_nameAnnotation("annotation");
const QString ods::ODSXmlContentsHandler::_nameText("p");

const QString ods::ODSXmlContentsHandler::_attValueType("office:value-type");
const QString ods::ODSXmlContentsHandler::_attValue("office:value");
const QString ods::ODSXmlContentsHandler::_attDateValue("office:date-value");
const QString ods::ODSXmlContentsHandler::_attTimeValue("office:time-value");
const QString ods::ODSXmlContentsHandler::_attBoolValue("office:boolean-value");
const QString ods::ODSXmlContentsHandler::_attCellRepeatCount("table:number-columns-repeated");
const QString ods::ODSXmlContentsHandler::_attRowRepeatCount("table:number-rows-repeated");

const QString ods::ODSXmlContentsHandler::_typeFloat("float");
const QString ods::ODSXmlContentsHandler::_typeCurrency("currency");
const QString ods::ODSXmlContentsHandler::_typePercent("percentage");
const QString ods::ODSXmlContentsHandler::_typeBoolean("boolean");
const QString ods::ODSXmlContentsHandler::_typeString("string");
const QString ods::ODSXmlContentsHandler::_typeDate("date");
const QString ods::ODSXmlContentsHandler::_typeTime("time");

namespace ods
{

ODSXmlContentsHandler::ODSXmlContentsHandler(ODSImportDataSet *dta)
		: XmlHandler(dta)
{
}

bool ODSXmlContentsHandler::parse(QXmlStreamReader &reader)
{
		while (!reader.atEnd() && !reader.hasError())
		{
			QXmlStreamReader::TokenType token = reader.readNext();
			switch (token)
			{
				case QXmlStreamReader::StartElement:
						processStartElement(reader);
						break;
				case QXmlStreamReader::EndElement:
						processEndElement(reader);
						break;
				case QXmlStreamReader::Characters:
						if (!reader.isWhitespace())
								processCharacters(reader.text().toString());
						break;
				default:
						break;
			}
		}
		return !reader.hasError();
}

void ODSXmlContentsHandler::processStartElement(const QXmlStreamReader &reader)
{
		if (_tableRead) return;

		QString localName = reader.name().toString();
		QXmlStreamAttributes atts = reader.attributes();

		switch(_docDepth)
		{
		case not_in_doc:
				if (localName == _nameDocContent) _docDepth = document_content;
				break;
		case document_content:
				if (localName == _nameBody) _docDepth = body;
				break;
		case body:
				if (localName == _nameSpreadsheet) _docDepth = spreadsheet;
				break;
		case spreadsheet:
				if (localName == _nameTable) _docDepth = table;
				break;
		case table:
				if (localName == _nameTableRow) {
					_docDepth = table_row;
					_rowRepeat = _findRowRepeat(atts);
				}
				break;
		case table_row:
				if (localName == _nameTableCell) {
					_docDepth = table_cell;
					_currentCell.clear();
					_setLastTypeGetValue(_currentCell, atts);
					_colRepeat = _findColRepeat(atts);
				}
				break;
		case table_cell:
				if (localName == _nameAnnotation) _docDepth = annotation;
				else if (localName == _nameText) _docDepth = text;
				break;
		case annotation:
				if (localName == _nameText) _docDepth = text_annotation;
				break;
		default: break;
		}
}

void ODSXmlContentsHandler::processEndElement(const QXmlStreamReader &reader)
{
		if (_tableRead) return;

		QString localName = reader.name().toString();

		switch(_docDepth)
		{
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
				if (localName == _nameTableRow) {
					_docDepth = table;
					//Repeat some rows but only do it if it *isnt* to make the data the same size as the max excel allows...
					if (_row > 0 && _lastNotEmptyColumn > -1 && (_row + _rowRepeat) < _excelMaxRows) {
						int sourceRowIdx = _row - 1; // XML source index

						for (int i = 1; i < _rowRepeat; i++)
						{
							int targetRowIdx = _row;
							_dataSet->createSpace(targetRowIdx);

							for (int j = 0; j < _dataSet->columnCount(); j++)
							{
								// from sourceRowIdx (the original cell) copy to targetRowIdx (repeat row)
								(*_dataSet)[j].setValue(targetRowIdx, (*_dataSet)[j].getCell(sourceRowIdx).valueAsString());
								(*_dataSet)[j].setComment(targetRowIdx, (*_dataSet)[j].getCell(sourceRowIdx).commentAsString());
							}
							_row++; // continue to last row after handled a repeat row
						}
					}

					_row++;
					// Starting next row.
					_column = 0;
					_lastNotEmptyColumn = -1;
					_currentCell.clear();
					_currentComment.clear();
					_colRepeat = 1;
					_rowRepeat = 1;
				}
				break;
		case table_cell:
				if (localName == _nameTableCell) {
						if(_row == 0) {
								// There is some celldata and we dont have any rows yet, so create headers:
								// Deals with header
								// First add columns that had no name/data
								if (!_currentCell.isEmpty()) {
									for (int i = _lastNotEmptyColumn + 1; i < _column; i++)
											_dataSet->createColumn("");

									// Avoid reference invalidation
									auto & col = _dataSet->createColumn(_currentCell.toStdString());
									if(!_currentComment.isEmpty())
										col.setTitle(fq(_currentComment));
									_lastNotEmptyColumn = _column;
								}
								_column += _colRepeat;
						} else {
								for (int i = _lastNotEmptyColumn + 1; i < _column; i++)
									_dataSet->getOrCreate(i).setValue(_row - 1, "");

								if((!_currentCell.isEmpty() || !_currentComment.isEmpty()) && _column + _colRepeat != _excelMaxCols) {
									for (int i = 0; i < _colRepeat; i++) {
											auto & col = _dataSet->getOrCreate(_column + i);
											col.setValue(_row - 1, fq(_currentCell));
											if(!_currentComment.isEmpty())
												col.setComment(_row - 1, fq(_currentComment));
									}
									_lastNotEmptyColumn = _column + _colRepeat - 1;
								}
								_column += _colRepeat;
						}
						_docDepth = table_row;
						_colRepeat = 1;
						_currentCell.clear();
						_currentComment.clear();
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
		default: break;
		}
}

void ODSXmlContentsHandler::processCharacters(const QString &ch)
{
		if (_tableRead || ch.isEmpty()) return;

		if (_docDepth == text) {
			if(_currentCell.isEmpty())
				_currentCell.push_back(ch);
		} else if (_docDepth == text_annotation) {
			if(!_currentComment.isEmpty())
				_currentComment.push_back("\t");
			_currentComment.push_back(ch);
		}
}

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

XmlDatatype ODSXmlContentsHandler::_setLastTypeGetValue(QString &value, const QXmlStreamAttributes &atts)
{
	_lastType = odsType_unknown;
	QString fromfile = atts.value(_attValueType).toString();

	if (fromfile == _typeFloat)			_lastType = odsType_float;
	else if (fromfile == _typeCurrency) _lastType = odsType_currency;
	else if (fromfile == _typePercent)	_lastType = odsType_percent;
	else if (fromfile == _typeBoolean)	_lastType = odsType_boolean;
	else if (fromfile == _typeDate)		_lastType = odsType_date;
	else if (fromfile == _typeTime)		_lastType = odsType_time;
	else if (fromfile == _typeString)	_lastType = odsType_string;

	switch(_lastType)
	{
	case odsType_float:
	case odsType_currency:
	case odsType_percent:
			value = atts.value(_attValue).toString();
			break;
	case odsType_boolean:
			value = atts.value(_attBoolValue).toString();
			break;
	case odsType_date:
			value = atts.value(_attDateValue).toString();
			break;
	case odsType_time:
			value = atts.value(_attTimeValue).toString();
			break;
	default:
			value.clear();
			break;
	}
	return _lastType;
}

int ODSXmlContentsHandler::_findColRepeat(const QXmlStreamAttributes &atts, int defaultValue)
{
	bool okay = false;
	int result = atts.value(_attCellRepeatCount).toInt(&okay);
	return okay ? result : defaultValue;
}

int ODSXmlContentsHandler::_findRowRepeat(const QXmlStreamAttributes &atts, int defaultValue)
{
	bool okay = false;
	int result = atts.value(_attRowRepeatCount).toInt(&okay);
	return okay ? result : defaultValue;
}

} // namespace ods
