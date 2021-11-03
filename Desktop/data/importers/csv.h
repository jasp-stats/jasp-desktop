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

#ifndef CSV_H
#define CSV_H

#include <vector>
#include <map>

#include <stdint.h>

#include <boost/nowide/fstream.hpp>

///
/// This files is used to read CSV files
/// It tries to guess the utf encoding from either a BOM if present (and has the option to decide it is then in the "local codepage" if running on windows)
/// And otherwise it just looks at the characters and sees if any of the codes for multiple bytes etc are present.
/// It also tries to determine the delimiter by looking at the first line and trying some fun heuristics.
/// If it finds nothing (one column for instance, or something crazy) it defaults to comma
class CSV
{
public:
	CSV(const std::string &path);

	void open();
	bool readLine(std::vector<std::string> &items);
	long pos();
	long size();
	void close();

	enum Status { OK = 0, NotRead, Empty };

	Status status();

private:

	long _fileSize;
	long _filePosition;

	enum Encoding { Unknown = -1, UTF8 = 0, UTF16BE = 1, UTF16LE = 2, UTF32LE = 3, UTF32BE = 4, Native };

    Encoding _encoding;
    char _delim;

	bool readRaw();
	bool readUtf8();

	void determineEncoding();
	void determineDelimiters(size_t fromHere = 0);

private:

	Status _status;

	int _rawBufferStartPos, _rawBufferEndPos;
	int _utf8BufferStartPos, _utf8BufferEndPos;
	std::string _path;
	boost::nowide::ifstream _stream;
	bool _eof;

	char _rawBuffer[32768];
	char _utf8Buffer[65536];

	static inline bool utf16to8(char *out, char *in, int outSize, int inSize, int &written, int &read, bool bigEndian = false);
	static inline bool utf16to32(uint32_t &out, char *in, int inSize, int &bytesRead, bool bigEndian = false);
	static inline bool utf32to8(char *out, uint32_t in, int outSize, int &bytesWritten);
};

#endif // CSV_H
