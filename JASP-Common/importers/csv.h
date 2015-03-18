#ifndef CSV_H
#define CSV_H

#include <fstream>
#include <vector>
#include <map>

#include <stdint.h>

class CSV
{
public:
	CSV(std::string path);

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

	enum Encoding { Unknown = -1, UTF8 = 0, UTF16BE = 1, UTF16LE = 2, UTF32LE = 3, UTF32BE = 4 };

    Encoding _encoding;
    char _delim;

	bool readRaw();
	bool readUtf8();

	void determineEncoding();
	void determineDelimiters();

private:

	Status _status;

	int _rawBufferStartPos, _rawBufferEndPos;
	int _utf8BufferStartPos, _utf8BufferEndPos;
	std::string _path;
	std::ifstream _stream;
	bool _eof;

	char _rawBuffer[4096];
	char _utf8Buffer[8192];

	static inline bool utf16to8(char *out, char *in, int outSize, int inSize, int &written, int &read, bool bigEndian = false);
	static inline bool utf16to32(uint32_t &out, char *in, int inSize, int &bytesRead, bool bigEndian = false);
	static inline bool utf32to8(char *out, uint32_t in, int outSize, int &bytesWritten);
};

#endif // CSV_H
