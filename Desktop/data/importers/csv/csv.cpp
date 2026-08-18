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
#include "csv.h"

#include <boost/algorithm/string.hpp>
#include "utilities/codepageswindows.h"
#include "utilities/settings.h"

#include <cstring>
#include <stdexcept>

#include "utils.h"

using namespace std;
using boost::algorithm::trim;

CSV::CSV(const string &path)
	: CSVParser(',', true)
{
	_encoding = UTF8;
	_delim = ',';
	_eof = false;

	_path = path;
	_fileSize = 0;
	_filePosition = 0;
	_numRows = -1;
}


void CSV::open()
{
	_fileSize = Utils::getFileSize(_path);

	if (_fileSize < 0)
		throw runtime_error("Could not access file");

	if (_fileSize == 0)
	{
		_status = Empty;
		throw runtime_error("File is empty");
	}

	_rawBufferStartPos = 0;
	_rawBufferEndPos = 0;
	_utf8BufferStartPos = 0;
	_utf8BufferEndPos = 0;

	_stream.open(_path.c_str(), ios::in);

	if ( ! _stream.is_open())
	{
		_status = Empty;
		throw runtime_error("Could not open file");
	}

	if (readRaw())
	{
		determineEncoding();
		determineNumRows();
		readUtf8();
		determineDelimiters();
	}
	else
	{
		_status = Empty;
	}

	//Log::log() << "encoding : " << _encoding << " delimeters : " << _delim << std::endl;
}

bool CSV::readRaw()
{	
	int bytesToMove = _rawBufferEndPos - _rawBufferStartPos;

	for (int i = bytesToMove - 1; i >= 0; i--)
		_rawBuffer[i] = _rawBuffer[_rawBufferStartPos + i];

	_rawBufferEndPos = bytesToMove;
	_rawBufferStartPos = 0;

	_stream.read(&_rawBuffer[_rawBufferEndPos], sizeof(_rawBuffer) - _rawBufferEndPos);
	int bytesRead = _stream.gcount();

	_filePosition += bytesRead;

	if (bytesRead == 0)
	{
		return false;
	}
	else
	{
		_rawBufferEndPos += bytesRead;
		return true;
	}
}

void CSV::determineEncoding()
{
	if (_rawBufferEndPos >= 4 && _rawBuffer[0] == -1 && _rawBuffer[1] == -2 && _rawBuffer[2] == 0 && _rawBuffer[3] == 0)
	{
		_encoding = UTF32LE;
		_rawBufferStartPos = 4;
	}
	else if (_rawBufferEndPos >= 4 && _rawBuffer[0] == 0 && _rawBuffer[1] == 0 && _rawBuffer[2] == -2 && _rawBuffer[3] == -1)
	{
		_encoding = UTF32BE;
		_rawBufferStartPos = 4;
	}
	else if (_rawBufferEndPos >= 2 && _rawBuffer[0] == -1 && _rawBuffer[1] == -2)
	{
		_encoding = UTF16LE;
		_rawBufferStartPos = 2;
	}
	else if (_rawBufferEndPos >= 2 && _rawBuffer[0] == -2 && _rawBuffer[1] == -1)
	{
		_encoding = UTF16BE;
		_rawBufferStartPos = 2;
	}
	else if (_rawBufferEndPos >= 3 && _rawBuffer[0] == -17 && _rawBuffer[1] == -69 && _rawBuffer[2] == -65)
	{
		_encoding = UTF8;
		_rawBufferStartPos = 3;
	}
	else
	{
#ifdef _WIN32
		//If we are on windows and there is no BOM, then we can assume it is encoded in the native locale. Unless the user decides otherwise
		if(Settings::value(Settings::WINDOWS_NO_BOM_NATIVE).toBool())
			_encoding = Native;
		else
#endif
		{
			// tab, lf, cr, space, double-quote, single-quote, comma, semi-colon
	
			uint16_t utf16be[] = { 0x0009, 0x000A, 0x000D, 0x0020, 0x0022, 0x0027, 0x002C, 0x003B };
			uint16_t utf16le[] = { 0x0900, 0x0A00, 0x0D00, 0x2000, 0x2200, 0x2700, 0x2C00, 0x3B00 };
	
			uint16_t *buffer = (uint16_t*)&_rawBuffer[_rawBufferStartPos];
			int count = (_rawBufferEndPos - _rawBufferStartPos) / 2;
	
			int beCount = 0;
			int leCount = 0;
	
			for (int i = 0; i < count; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					if (buffer[i] == utf16be[j])
					{
						beCount++;
						break;
					}
					if (buffer[i] == utf16le[j])
					{
						leCount++;
						break;
					}
				}
			}
	
			if (beCount > leCount)
				_encoding = UTF16LE;
			else if (leCount > 1)
				_encoding = UTF16BE;
			else
				_encoding = UTF8;
		}
	}
}

bool CSV::readUtf8()
{
	if (_rawBufferEndPos == _rawBufferStartPos)
	{
		bool success = readRaw();

		if ( ! success)
			return false;
	}
	else if ((_rawBufferEndPos - _rawBufferStartPos) <= 2) // in case a lead surrogate was left behind
	{
		readRaw();
	}

	int bytesToMove = _utf8BufferEndPos - _utf8BufferStartPos;

	for (int i = bytesToMove - 1; i >= 0; i--)
		_utf8Buffer[i] = _utf8Buffer[_utf8BufferStartPos + i];

	_utf8BufferEndPos = bytesToMove;
	_utf8BufferStartPos = 0;

	switch(_encoding)
	{
	
	default:
	{
		int written, read;

		bool success = utf16to8(
			&_utf8Buffer[_utf8BufferEndPos],
			&_rawBuffer[_rawBufferStartPos],
			sizeof(_utf8Buffer) - _utf8BufferEndPos,
			_rawBufferEndPos - _rawBufferStartPos,
			written,
			read,
			_encoding == UTF16BE);

		if ( ! success)
			return false;

		_utf8BufferEndPos += written;
		_rawBufferStartPos += read;
		
		break;
	}
		
	case UTF8:
	{
		std::memcpy(&_utf8Buffer[_utf8BufferEndPos], &_rawBuffer[_rawBufferStartPos], _rawBufferEndPos - _rawBufferStartPos);

		_utf8BufferEndPos += _rawBufferEndPos - _rawBufferStartPos;
		_rawBufferStartPos = _rawBufferEndPos;

		break;
	}
		
	case Native:
	{
		std::string raw    (&_rawBuffer[_rawBufferStartPos], &_rawBuffer[_rawBufferEndPos]),
					utf8(CodePagesWindows::convertCodePageStrToUtf8(raw));
		
		std::memcpy(&_utf8Buffer[_utf8BufferEndPos], utf8.c_str(), utf8.size());

		_utf8BufferEndPos += utf8.size();
		_rawBufferStartPos = _rawBufferEndPos;
		
		break;
	}
	}

	for (int i = 0 ; i < _utf8BufferEndPos; i++)
	{
		if ((unsigned char)_utf8Buffer[i] < 0x80) // ascii
		{
			continue;
		}
		else if ((unsigned char)_utf8Buffer[i] < 0xC0) // illegal
		{
			_utf8Buffer[i] = '.';
		}
		else if ((unsigned char)_utf8Buffer[i] < 0xE0) // 2 bytes
		{
			if (i < _utf8BufferEndPos - 1 && (unsigned char)_utf8Buffer[i+1] < 0x80)
				_utf8Buffer[i] = '.';
			else
				i += 1;
		}
		else if ((unsigned char)_utf8Buffer[i] < 0xF0) // 3 bytes
		{
			if (i < _utf8BufferEndPos - 2 && (unsigned char)_utf8Buffer[i+1] < 0x80 && (unsigned char)_utf8Buffer[i+2] < 0x80)
				_utf8Buffer[i] = '.';
			else
				i += 2;
		}
		else if ((unsigned char)_utf8Buffer[i] < 0xF8) // 4 bytes
		{
			if (i < _utf8BufferEndPos - 3 && (unsigned char)_utf8Buffer[i+1] < 0x80 && (unsigned char)_utf8Buffer[i+2] < 0x80 && (unsigned char)_utf8Buffer[i+3] < 0x80)
				_utf8Buffer[i] = '.';
			else
				i += 3;
		}
		else
		{
			_utf8Buffer[i] = '.';
		}
	}

	return true;
}


void CSV::determineDelimiters(size_t fromHere)
{
	bool    inQuote        = false,
			eol            = false;
	int     semicolons    = 0,
			commas        = 0,
			spaces        = 0,
			tabs          = 0,
			stopped        = 0;

	for (int i = fromHere; i < _utf8BufferEndPos && eol == false; i++)
	{
		char ch = _utf8Buffer[i];

		if (ch == '"')
		{
			if (inQuote && i + 1 < _utf8BufferEndPos && _utf8Buffer[i + 1] == '"')
				i++;
			else
				inQuote = !inQuote;

			continue;
		}

		if (inQuote)
			continue;

		switch (ch)
		{
		case ',':
			commas++;
			break;
		case ';':
			semicolons++;
			break;
		case ' ':
			spaces++;
			break;
		case '\t':
			tabs++;
			break;
		case '\r':
		case '\n':
			eol        = true;
			stopped = i;
			while(stopped < _utf8BufferEndPos && (_utf8Buffer[stopped] == '\r' || _utf8Buffer[stopped] == '\n'))
				stopped++;
			break;
		}
	}

	_delim = commas == 0 ? ';' : ',';
	int countDelim = commas; 
	
	if (semicolons > countDelim)
	{
		_delim = ';';
		countDelim = semicolons;
	}
	if (tabs > countDelim)
	{
		_delim = '\t';
		countDelim = tabs;
	}
	if (countDelim == 0 && spaces > 0) // uses spaces only if there is nothing else.
	{
		//See https://github.com/jasp-stats/jasp-test-release/issues/1040 for problems with single column-csv that contain a space in the title.
		
		if(fromHere == 0) //We just checked the first line, maybe the second line is more useful?
			determineDelimiters(stopped);
		else //The second line was as useless as the first one apparently.
			_delim = ' ';
	}
}

void CSV::determineNumRows()
{
	_numRows = 0;
	bool eof = true;

	if (_utf8BufferEndPos == _utf8BufferStartPos)
	{
		eof = !readUtf8();
	}

	int i = _utf8BufferStartPos;
	while (!eof)
	{
		char ch = _utf8Buffer[i];

		if (ch == '\r')
		{
			_numRows++;
			if (i + 1 < _utf8BufferEndPos && _utf8Buffer[i + 1] == '\n')
				_utf8BufferStartPos = i + 2;
			else
				_utf8BufferStartPos = i + 1;
		}
		else if (ch == '\n')
		{
			_utf8BufferStartPos = i + 1;
			_numRows++;
		}

		if (i >= _utf8BufferEndPos - 1)
		{
			_utf8BufferEndPos = 0;
			bool success = readUtf8();
			if (success)
				i = -1;
			else // eof
				eof = true;
		}
		i++;
	}

	//disregard header
	_numRows--;

	//reset state before starting this whole thing
	_filePosition = 0;
	_stream.clear();
	_stream.seekg(0, std::ios::beg);

	_rawBufferStartPos = 0;
	_rawBufferEndPos = 0;
	_utf8BufferStartPos = 0;
	_utf8BufferEndPos = 0;

	if (readRaw())
	{
		determineEncoding();
		readUtf8();
		determineDelimiters();
	}

}

bool CSV::readLine(vector<string> &items)
{
	if (_eof)
		return false;

	if (_utf8BufferEndPos == _utf8BufferStartPos)
	{
		if (!readUtf8())
		{
			_eof = true;
			return false;
		}
	}

	// Process characters through the parser until row is complete
	size_t startPos = _utf8BufferStartPos;
	size_t i = startPos;

	while (i < _utf8BufferEndPos)
	{
		char ch = _utf8Buffer[i];

	// Replace illegal UTF-8 bytes with '.' (same as original logic)
	if ((unsigned char)ch >= 0xF8)
		ch = '.';

	if (processChar(ch))
	{
		// Same char should be re-processed - don't increment i
		if (hasRow())
		{
			items = extractRow();
			_utf8BufferStartPos = i + 1;
			reset();
			return !items.empty();
		}
		_utf8BufferStartPos = i; // Same char will be re-processed
		continue;
	}

	if (hasRow())
	{
		// Row complete - extract it
		items = extractRow();
		_utf8BufferStartPos = i + 1;
		reset();
		return !items.empty();
	}

		i++;

		// If we reached the end of buffer, try to load more
		if (i >= _utf8BufferEndPos)
		{
			_utf8BufferStartPos = i; // Only unprocessed bytes from here
			if (!readUtf8())
			{
				// EOF - process remaining data
				while (i < _utf8BufferEndPos)
				{
					char ch = _utf8Buffer[i];
					if ((unsigned char)ch >= 0xF8)
						ch = '.';
					processChar(ch);
					i++;
				}

				items = extractRow();
				_utf8BufferStartPos = i;
				reset();
				
				_eof = true;
				return !items.empty();
			}
			i = 0;
		}
	}

	// Shouldn't reach here normally, but just in case
	_eof = true;
	items.clear();
	return false;
}

int64_t CSV::pos()
{
	return _filePosition;
}

int64_t CSV::size()
{
	return _fileSize;
}

int64_t CSV::numRows()
{
	return _numRows > 0 ? _numRows : 0;
}

void CSV::close()
{
	_stream.close();
}

string CSV::firstRowsPlease()
{
	//This should return the first couple of rows (including the header with the columns) only.
	string snippet;
	int rowsRead = 0;
	const int maxRows = 20;

	while (rowsRead < maxRows)
	{
		string line = readLineRaw();
		if (line.empty())
			break;
		snippet += line + "\n";
		rowsRead++;
	}

	// Reset all state so the caller can read from the beginning normally.
	// Must mirror determineNumRows() so that BOM bytes are skipped and encoding
	// is properly accounted for before the next readLine() call.
	_filePosition = 0;
	_eof = false;
	_stream.clear();
	_stream.seekg(0, std::ios::beg);

	_rawBufferStartPos = 0;
	_rawBufferEndPos   = 0;
	_utf8BufferStartPos = 0;
	_utf8BufferEndPos   = 0;

	if (readRaw())
	{
		determineEncoding();
		readUtf8();
		determineDelimiters();
	}

	return snippet;
}

bool CSV::utf16to8(char *out, char *in, int outSize, int inSize, int &written, int &read, bool bigEndian)
{
	written = 0;
	read = 0;
	bool success;

	while (true)
	{
		int bytesLeftToRead = inSize - read;
		int roomLeftToWrite = outSize - written;

		uint32_t ch;

		int justRead;
		int justWritten;

		success = utf16to32(ch, &in[read], bytesLeftToRead, justRead, bigEndian);
		if ( ! success)
			break;

		success = utf32to8(&out[written], ch, roomLeftToWrite, justWritten);
		if ( ! success)
			break;

		read += justRead;
		written += justWritten;
	}

	return read > 0 && written > 0;
}

bool CSV::utf16to32(uint32_t &out, char *in, int inSize, int &bytesRead, bool bigEndian)
{

#define UNI_SUR_HIGH_START      (uint32_t)0xD800
#define UNI_SUR_HIGH_END        (uint32_t)0xDBFF
#define UNI_SUR_LOW_START       (uint32_t)0xDC00
#define UNI_SUR_LOW_END         (uint32_t)0xDFFF
#define UNI_HALF_SHIFT          (uint32_t)10
#define UNI_HALF_BASE           (uint32_t)0x0010000UL
#define UNI_HALF_MASK           (uint32_t)0x3FFUL

	if (inSize < 2)
		return false;

	uint32_t upper;

	if (bigEndian)
	{
		upper = in[0];
		upper <<= 8;
		upper |= in[1];
	}
	else
	{
		upper = *(uint16_t*)(in);
	}

	if ((uint32_t)(*in) >= UNI_SUR_HIGH_START && (uint32_t)(*in) <= UNI_SUR_LOW_START)
	{
		if (inSize < 4)
			return false;

		uint32_t lower;

		if (bigEndian)
		{
			lower = in[3];
			lower <<= 8;
			lower |= in[2];
		}
		else
		{
			lower = *(uint16_t*)(in + 2);
		}

		if (lower >= UNI_SUR_LOW_START && lower <= UNI_SUR_LOW_END)
		{
			out = ((upper - UNI_SUR_HIGH_START) << UNI_HALF_SHIFT) + (lower - UNI_SUR_LOW_START) + UNI_HALF_BASE;
		}

		bytesRead = 4;
		return true;
	}
	else
	{
		out = upper;

		bytesRead = 2;
		return true;
	}
}

bool CSV::utf32to8(char *out, uint32_t in, int outSize, int &bytesWritten)
{
	int width;
	const unsigned char FIRST_BYTE_MARK[7] = {0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC};

	if (in < 0x80)
	{
		width = 1;
	}
	else if (in < 0x800)
	{
		width = 2;
	}
	else if (in < 0x10000)
	{
		width = 3;
	}
	else if (in < 0x110000)
	{
		width = 4;
	}
	else
	{
		in = 0xFFFD;  // replacement character
		width = 3;
	}

	if (width > outSize)
		return false;

	switch (width)
	{
	case 4:
		out[3] = (char)((in | 0x80) & 0xBF);
		in >>= 6;
	case 3:
		out[2] = (char)((in | 0x80) & 0xBF);
		in >>= 6;
	case 2:
		out[1] = (char)((in | 0x80) & 0xBF);
		in >>= 6;
	case 1:
		out[0] = (char)(in | FIRST_BYTE_MARK[width]);
	}

	bytesWritten = width;

	return true;
}

std::string CSV::readLineRaw()
{
	if (_eof)
		return "";

	if (_utf8BufferEndPos == _utf8BufferStartPos)
	{
		if (!readUtf8())
			return "";
	}

	std::string result;

	while (true)
	{
		// Scan the current buffer for a line terminator
		int end = -1;
		for (int i = _utf8BufferStartPos; i < _utf8BufferEndPos; ++i)
		{
			if (_utf8Buffer[i] == '\n' || _utf8Buffer[i] == '\r')
			{
				end = i;
				break;
			}
		}

		if (end != -1)
		{
			result.append(&_utf8Buffer[_utf8BufferStartPos], end - _utf8BufferStartPos);

			bool hadCR = (_utf8Buffer[end] == '\r');

			if (hadCR && end + 1 < _utf8BufferEndPos && _utf8Buffer[end + 1] == '\n')
				_utf8BufferStartPos = end + 2;  // consume \r\n together
			else
				_utf8BufferStartPos = end + 1;  // consume lone \r or \n

			// Handle \r\n split across a buffer boundary: \r was the last byte,
			// peek into the next buffer to consume the \n if it's there.
			if (hadCR && _utf8BufferStartPos == _utf8BufferEndPos)
			{
				if (readUtf8() && _utf8Buffer[_utf8BufferStartPos] == '\n')
					_utf8BufferStartPos++;
			}

			return result;
		}

		// No newline in current buffer — accumulate it and load more data
		result.append(&_utf8Buffer[_utf8BufferStartPos], _utf8BufferEndPos - _utf8BufferStartPos);
		_utf8BufferStartPos = _utf8BufferEndPos;  // mark all bytes consumed

		if (!readUtf8())
		{
			// EOF: return the last line even without a trailing newline
			_eof = true;
			return result;
		}
	}
}
