/*
    Copyright (C) Copyright (C) 2013-2017 University of Amsterdam

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


    File created by patrick, on 08-08-2017
    Original file name was
*/

#ifndef SYSDEPFILETYPE_H
#define SYSDEPFILETYPE_H 1


#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

//#ifdef _WIN32
//  #define JASPBOOST_IFSTREAM_SUPER boost::filesystem::wifstream
//  #define JASPBOOST_OFSTREAM_SUPER boost::filesystem::wofstream
//#else
  #define JASPBOOST_IFSTREAM_SUPER boost::filesystem::ifstream
  #define JASPBOOST_OFSTREAM_SUPER boost::filesystem::ofstream
//#endif

class JaspIfStream : public JASPBOOST_IFSTREAM_SUPER
{
public:
    explicit JaspIfStream()
        : JASPBOOST_IFSTREAM_SUPER() {}

    explicit JaspIfStream(const boost::filesystem::path& p)
      : JASPBOOST_IFSTREAM_SUPER(p) {}

    JaspIfStream(const boost::filesystem::path& p, std::ios_base::openmode mode)
      : JASPBOOST_IFSTREAM_SUPER(p, mode) {}

    // The std librates assume that the char type for the
    // read and write buffers are the same as for the
    // file names.

    /**
     *  @brief  Extraction without delimiters.
     *  @param  __s  A character array.
     *  @param  __n  Maximum number of *8 bit* characters to store.
     *  @return  *this
     *
     *  If the stream state is @c good(), extracts *8 bit* characters and stores
     *  them into @a __s until one of the following happens:
     *  - @a __n *8 bit* characters are stored
     *  - the input sequence reaches end-of-file, in which case the error
     *    state is set to @c failbit|eofbit.
     *
     */
    __istream_type & read(char *__s, std::streamsize __n);

};


class JaspFileTypes
{
public:
    typedef boost::filesystem::path     FilePath;
    typedef JASPBOOST_OFSTREAM_SUPER	OFStream;
    typedef JaspIfStream                IFStream;
};

#endif // SYSDEPFILETYPE_H
