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


#include "boost/nowide/fstream.hpp"
#include "boost/filesystem/path.hpp"
#include <string>
#include <qstring.h>

namespace JaspFiles {

class _jaspPath
{
	friend class _jaspifstream;
	friend class _jaspostream;

	std::string _s;		// The working bit.
public:

/**
 * @brief DIR_SEP The directory seperator for this OS.
 */
#ifdef _WIN32
  #define _DIR_SEP '\\'
#else
  #define _DIR_SEP '/'
#endif
	static const unsigned char QT_DIR_SEP = '/';
	static const unsigned char DIR_SEP =  _DIR_SEP;
	static const unsigned char DOT =  '.';


	/**
	 * @brief _jaspPath default Ctor.
	 */
	_jaspPath() {}

	/**
	 * @brief toPath Ctor from a std::string file path (UTF-8).
	 * @param path The std::string path to convert.
	 * @return A path object with the same content as the param `path`.
	 */
	_jaspPath(const std::string &path)
		: _s(path)
	{}

	/**
	 * @brief toPath Ctor from a std::string file path (UTF-8).
	 * @param path The path to convert.
	 * @return A path object with the same content as the param `path`.
	 */
	_jaspPath(const char *path)
		: _s(path)
	{}

	/**
	 * @brief toPath Ctor from a std::string file path (UTF-8).
	 * @param path The path to convert.
	 * @return A path object with the same content as the param `path`.
	 */
	_jaspPath(const boost::filesystem::path &path)
		: _s(path.string())
	{}

	/**
	 * @brief toPath Ctor from a QString file path.
	 *@param pa The QString path to convert.
	* @return A path object with the same content as the param `path`.
	*/
	_jaspPath(const QString &path)
		: _s(cvt(path))
	{}


	/**
	 * @brief operator /= Appends a directory level to the right.
	 * @param rhs The name of the directory to add.
	 * @return *this;
	 */
	_jaspPath & operator /= (const std::string &rhs);
	_jaspPath & operator /= (const char *rhs);

	/**
	 * @brief string Get path as UTF-8 string.
	 * @return
	 */
	const std::string & string() const { return _s; }

	/**
	 * @brief string Get path as UTF-8 string.
	 * @return
	 */
	const char * c_str() const { return  _s.c_str(); }

	/**
	 * @brief string Get path as UTF-16 string.
	 * @return
	 */
	std::wstring wstring() const;

	/**
	 * @brief string Get path as UTF-16 string.
	 * @return
	 */
	QString qstring() const;

	/**
	 * @brief toPath Returns a path
	 * @return path.
	 */
	boost::filesystem::path  path() const;

	/**
	 * @brief empty expose super's `empty()`.
	 * @return as super's `empty()`.
	 */
	bool empty() const { return _s.empty(); }

	/**
	 * @brief startsWith compaires fist chars in path
	 * @param search Comparison.
	 * @return `true` if the first chars in path match `search` exactly.
	 */
	bool startsWith(const std::string &search) const;

	/**
	 * @brief filename Returns the filename portion.
	 * @return The file-name.
	 */
	std::string filename() const;

	/**
	 * @brief extension Get the extension.
	 * @return The extension.
	 */
	std::string extension() const;

	/**
	 * @brief has_extension Returns true if there is DOT anything in filename.
	 * @return
	 */
	bool has_extension() const;

#ifdef _WIN32
	/**
	 * @brief parent_path Returns the enclosing directory.
	 * @return The directory "one up".
	 *
	 * Only used in Windows test builds
	 */
	_jaspPath parent_path() const;
#endif

protected:
	/**
	 * @brief toQStr Convert to QString
	 * @return The file name in OS native format as QString
	 */
	static std::string cvt(const QString & from);
};

// The type used for file paths JASP (desktop) wide.
typedef _jaspPath	Path;


class _jaspifstream : public boost::nowide::ifstream
{
public:
	/**
	 * @brief _jaspifstream default Ctor
	 */
	_jaspifstream() : boost::nowide::ifstream() {}

	/**
	 * @brief _jaspifstream Ctor from a Path (aka string)
	 * @param filename  The file name (UTF-8).
	 * @param mode The file open mode.
	 */
	_jaspifstream(const Path & filename, ios_base::openmode mode = ios_base::in)
		: boost::nowide::ifstream(filename.string().c_str(), mode)
	{}


	/**
	 *  @brief  Opens an external file.
	 *  @param  __s  The name of the file.
	 *  @param  __mode  The open mode flags.
	 *
	 *  Calls @c std::basic_filebuf::open(__s,__mode|in).  If that function
	 *  fails, @c failbit is set in the stream's error state.
	 */
	void
	open(const Path & __s, ios_base::openmode __mode = ios_base::in)
		{ boost::nowide::ifstream::open(__s.string().c_str(), __mode); }

};

class _jaspofstream : public boost::nowide::ofstream
{
public:
	/**
	 * @brief _jaspofstream Ctor from a Path (aka string)
	 * @param filename  The file name (UTF-8).
	 * @param mode The file open mode.
	 */
	_jaspofstream(const Path & filename, ios_base::openmode mode = ios_base::out)
		: boost::nowide::ofstream(filename.string().c_str(), mode)
	{}

	/**
	 * @brief _jaspofstream  default Ctor
	 */
	_jaspofstream()
		: boost::nowide::ofstream()
	{}

	/**
	 *  @brief  Opens an external file.
	 *  @param  __s  The name of the file.
	 *  @param  __mode  The open mode flags.
	 *
	 *  Calls @c std::basic_filebuf::open(__s,__mode|in).  If that function
	 *  fails, @c failbit is set in the stream's error state.
	 */
	void
	open(const Path & __s, ios_base::openmode __mode = ios_base::in)
		{ boost::nowide::ofstream::open(__s.string().c_str(), __mode); }
};

typedef _jaspofstream	OFStream;
typedef _jaspifstream	IFStream;

} // end namespace JaspFiles

#endif // SYSDEPFILETYPE_H
