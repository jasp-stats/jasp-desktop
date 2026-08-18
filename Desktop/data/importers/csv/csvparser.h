//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef CSVPARSER_H
#define CSVPARSER_H

#include <queue>
#include <string>
#include <QString>
#include <QObject>
#include <utils.h>

///
/// CSVParser: Pure state machine for CSV parsing (RFC 4180 compliant)
/// Handles:
/// - Quoted fields (fields enclosed in double quotes)
/// - Escaped quotes ("" within quoted fields)
/// - Line endings within quoted fields (CRLF, LF, or CR)
/// - Delimiters within quoted fields (preserved as part of field content)
///
/// This is a utility class used by CSV (for file streaming) and CsvPreviewModel (for batch parsing)
/// No file I/O, no encoding detection - just the parsing state machine.
class CSVParser : public QObject
{
	Q_OBJECT

public:
	typedef std::vector<std::vector<std::string>> Grid;

	/// Constructor
	/// @param delimiter The field delimiter character
	/// @param replaceLineEndings If true, replace all line endings (\r, \n, \r\n) with spaces in field content
	CSVParser(char delimiter, bool replaceLineEndings = true);

	/// Parse a complete CSV string
	/// @param data The CSV data as a string
	Grid parse(const std::string& data);

	/// Parse a complete CSV string from QString
	/// @param data The CSV data as a QString
	Grid parse(const QString& data);

	/// Process a single character (for streaming)
	/// @param ch The character to process
	/// @return true if the same character should be re-processed, false otherwise
	bool processChar(char ch);

	/// Check if a complete row has been parsed
	/// @return true if at least one complete row is available
	bool hasRow() const;

	/// Extract the next complete row
	/// @return Vector of field strings for the row
	std::vector<std::string> extractRow();



	/// Get number of rows parsed
	/// @return Row count
	size_t getRowCount() const;

	/// Check if parser has any pending data
	/// @return true if there's unextracted data
	bool hasPendingData() const;

	/// Reset parser state (clear all data, keeps delimiter)
	void reset();

	/// Change the delimiter and reset parser
	/// @param delimiter New delimiter character
	void setDelimiter(char delimiter);

	/// Get current delimiter
	char delimiter() const { return _delimiter; }
	
	
protected:
	/// Get all parsed rows
	/// @return Reference to the grid containing all parsed data, wipes the queue!
	Grid getGrid();

signals:
	void rowParsed(const stringvec & row);
	void parsingComplete();

private:
	enum State { Normal, Quoted, QuotedQuote };

	State						_state;
	std::string					_currentField;
	std::vector<std::string>	_currentRow;
	std::queue<stringvec>		_gridQueue;
	char						_delimiter;
	bool						_replaceLineEndings;
	bool						_rowFinished;
	bool						_skipNextLF;

	/// Finish current field (add to row, reset field)
	void finishField();

	/// Finish current row (add to grid, reset row)
	void finishRow();

	/// Apply line ending replacement to a field
	void replaceLineEndings(std::string& field) const;
};

#endif // CSVPARSER_H
