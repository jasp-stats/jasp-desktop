//
// Copyright (C) 2018 University of Amsterdam
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

#include "exporter.h"

Exporter::~Exporter() {}

Utils::FileType Exporter::getDefaultFileType()
{
	return _defaultFileType;
}

bool Exporter::isFileTypeAllowed(Utils::FileType filetype) {
	for (Utils::FileTypeVector::const_iterator i = _allowedFileTypes.begin(); i != _allowedFileTypes.end(); ++i) {
		if (*i == filetype) return true;
	}
	return false;
}

Utils::FileTypeVector Exporter::getAllowedFileTypes() {
	return _allowedFileTypes;
}

bool Exporter::setFileType(Utils::FileType filetype) {
	if (isFileTypeAllowed(filetype)) {
		_currentFileType = filetype;
		return true;
	}
	else
	{
		return false;
	}
}
