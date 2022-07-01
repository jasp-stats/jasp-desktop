//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "csv.h"

#include <boost/algorithm/string.hpp>


#include <cstring>
#include <stdexcept>

#include "utils.h"
#include "qutils.h"
#include "settings.h"

using namespace std;
using boost::algorithm::trim;

CSV::CSV(const string &path)
{
    _encoding = UTF8;
    _delim = ',';
	_eof = false;

    _path = path;
	_fileSize = 0;
	_filePosition = 0;
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
		std::string raw	(&_rawBuffer[_rawBufferStartPos], &_rawBuffer[_rawBufferEndPos]),
					utf8(QString::fromLocal8Bit(raw.c_str()).toStdString());
		
		std::memcpy(&_utf8Buffer[_utf8BufferEndPos], utf8.c_str(), utf8.size());

		_utf8BufferEndPos += _rawBufferEndPos - _rawBufferStartPos;
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
	bool	inQuote		= false,
			eol			= false;
	int		semicolons	= 0,
			commas		= 0,
			spaces		= 0,
			tabs		= 0,
			stopped		= 0;

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
			eol		= true;
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

bool CSV::readLine(vector<string> &items)
{
	if (_eof)
		return false;

	if (_utf8BufferEndPos == _utf8BufferStartPos)
	{
		bool success = readUtf8();
		if ( ! success)
			return false;
	}

	bool inQuote = false;

	int i = _utf8BufferStartPos;

	while (true)
	{
		char ch = _utf8Buffer[i];

		if ((unsigned char)ch >= 0xF8)  // illegal utf-8
		{
			ch = '.';
			_utf8Buffer[i] = '.';
		}

		if (ch == '"')
		{
			if (inQuote && i + 1 < _utf8BufferEndPos && _utf8Buffer[i + 1] == '"')
				i++;
			else
				inQuote = !inQuote;
		}

		if (inQuote)
		{
			// do nothing
		}
		else if (ch == _delim)
		{
			string token(&_utf8Buffer[_utf8BufferStartPos], i - _utf8BufferStartPos);
			trim(token);

			items.push_back(token);
			_utf8BufferStartPos = i + 1;
		}
		else if (ch == '\r')
		{
            if (items.size() > 0 || i > _utf8BufferStartPos) {
                string token(&_utf8Buffer[_utf8BufferStartPos], i - _utf8BufferStartPos);
                trim(token);
                items.push_back(token);
            }

			if (i + 1 < _utf8BufferEndPos && _utf8Buffer[i + 1] == '\n')
				_utf8BufferStartPos = i + 2;
			else
				_utf8BufferStartPos = i + 1;

            if (items.size() > 0)
                break;
		}
		else if (ch == '\n')
		{
            if (items.size() > 0 || i > _utf8BufferStartPos) {
                string token(&_utf8Buffer[_utf8BufferStartPos], i - _utf8BufferStartPos);
                trim(token);
                items.push_back(token);
            }

			_utf8BufferStartPos = i + 1;

            if (items.size() > 0)
                break;
		}

		if (i >= _utf8BufferEndPos - 1)
		{
			bool success = readUtf8();
			if (success)
			{
				i = -1;
				inQuote = false;
			}
			else // eof
			{
                if (items.size() > 0 || _utf8BufferEndPos > _utf8BufferStartPos) {
                    string token(&_utf8Buffer[_utf8BufferStartPos], _utf8BufferEndPos - _utf8BufferStartPos);
                    trim(token);
                    items.push_back(token);
                }
				_eof = true;
				break;
			}
		}

		i++;
	}

    for (size_t index = 0; index < items.size(); index++)
	{
        string item = items.at(index);
		if (item.size() >= 2 && item[0] == '"' && item[item.size()-1] == '"')
			item = item.substr(1, item.size()-2);
        items[index] = item;
	}

	return true;
}

long CSV::pos()
{
	return _filePosition;
}

long CSV::size()
{
	return _fileSize;
}

void CSV::close()
{
	_stream.close();
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

bool CSV::utf16to32(uint32_t &out, char *in, int inSize, int& bytesRead, bool bigEndian)
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
