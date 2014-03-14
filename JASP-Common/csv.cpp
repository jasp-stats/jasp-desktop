
#include "csv.h"

#include <sys/stat.h>
#include <iostream>
#include <cstring>

using namespace std;

CSV::CSV(string path)
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
	struct stat fileInfo;
	stat(_path.c_str(), &fileInfo);

	_fileSize = fileInfo.st_size;

	if (_fileSize == 0)
	{
		_status = Empty;
		return;
	}

	_rawBufferStartPos = 0;
	_rawBufferEndPos = 0;
	_utf8BufferStartPos = 0;
	_utf8BufferEndPos = 0;

	_stream.open(_path.c_str(), ios::in);

	if (readRaw())
	{
		determineEncoding();
		readUtf8();
		determineDelimiters();
	}
	else
		_status = Empty;

	std::cout << "encoding : " << _encoding << " delimeters : " << _delim << "\n";
	std::cout.flush();
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

	if (_encoding == UTF16BE || _encoding == UTF16LE)
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
	}
	else
	{
		std::memcpy(&_utf8Buffer[_utf8BufferEndPos], &_rawBuffer[_rawBufferStartPos], _rawBufferEndPos - _rawBufferStartPos);

		_utf8BufferEndPos += _rawBufferEndPos - _rawBufferStartPos;
		_rawBufferStartPos = _rawBufferEndPos;
	}

	return true;
}


void CSV::determineDelimiters()
{
	bool inQuote = false;

	enum { COMMA = 0, SEMICOLON = 1, SPACE = 2 };

	int counts[] = { 0, 0, 0 };

	for (int i = 0; i < _utf8BufferEndPos; i++)
	{
		char ch = _utf8Buffer[i];

		switch (ch)
		{
		case '"':
			if (inQuote && i + 1 < _utf8BufferEndPos && _utf8Buffer[i + 1] == '"')
				i++;
			else
				inQuote = !inQuote;
			break;
		case ',':
			counts[COMMA]++;
			break;
		case ';':
			counts[SEMICOLON]++;
			break;
		case ' ':
			counts[SPACE]++;
			break;
		}
	}

	int maxi = 0;
	for (int i = 1; i < 3; i++)
	{
		if (counts[maxi] < counts[i])
			maxi = i;
	}

	switch (maxi)
	{
	case SEMICOLON:
		_delim = ';';
		break;
	case SPACE:
		_delim = ' ';
		break;
	default:
		_delim = ',';
		break;
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
			items.push_back(token);
			_utf8BufferStartPos = i + 1;
		}
		else if (ch == '\r')
		{
			string token(&_utf8Buffer[_utf8BufferStartPos], i - _utf8BufferStartPos);
			items.push_back(token);

			if (i + 1 < _utf8BufferEndPos && _utf8Buffer[i + 1] == '\n')
				_utf8BufferStartPos = i + 2;
			else
				_utf8BufferStartPos = i + 1;

			break;
		}
		else if (ch == '\n')
		{
			string token(&_utf8Buffer[_utf8BufferStartPos], i - _utf8BufferStartPos);
			items.push_back(token);

			_utf8BufferStartPos = i + 1;
			break;
		}

		if (i >= _utf8BufferEndPos - 1)
		{
			bool success = readUtf8();
			if (success)
			{
				i = 0;
			}
			else // eof
			{
				string token(&_utf8Buffer[_utf8BufferStartPos], _utf8BufferEndPos - _utf8BufferStartPos);
				items.push_back(token);
				_eof = true;
				break;
			}
		}

		i++;
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
	int i;
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


	if (*in >= UNI_SUR_HIGH_START && *in <= UNI_SUR_LOW_START)
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
