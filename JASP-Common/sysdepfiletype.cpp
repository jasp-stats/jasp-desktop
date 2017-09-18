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


    File created by patrick, on 12-08-2017
    Original file name was sysdepfiletype.h
*/



#include "sysdepfiletype.h"

#ifndef QT_NO_DEBUG
#include <iostream>
#endif

using namespace std;
using namespace boost::filesystem;

/**
 *  @brief  Extraction without delimiters.
 *  @param  __s  A character array.
 *  @param  __n  Maximum number of *8 bit* characters to store.
 *  @return  *this
 *
 *  If the stream state is @c good(), extracts characters and stores
 *  them into @a __s until one of the following happens:
 *  - @a __n characters are stored
 *  - the input sequence reaches end-of-file, in which case the error
 *    state is set to @c failbit|eofbit.
 *
 */
JASPBOOST_IFSTREAM_SUPER::__istream_type &
JaspIfStream::read(char *__s, streamsize __n)
{
    streamsize numChars = 0;
    if (sizeof(char_type) > sizeof(char))
    {
        numChars = (__n * sizeof(char)) / sizeof(char_type);
        if ((__n % sizeof(char_type)) != 0)
        {
#ifndef QT_NO_DEBUG
            cout << "JaspIfStream::read(char *, streamsize): Found " << (__n % sizeof(char_type)) << " bytes over." << endl;
#endif
            numChars++;
        }
    }
    else
        numChars = __n;

    JASPBOOST_IFSTREAM_SUPER::read((JASPBOOST_IFSTREAM_SUPER::char_type *) __s, numChars);

    return *this;
}
