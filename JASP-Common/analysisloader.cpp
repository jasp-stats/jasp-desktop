//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "analysisloader.h"
#include "sysdepfiletype.h"

#include <string>
#include "dirs.h"
#include "version.h"
#include "sysdepfiletype.h"

#ifndef QT_NO_DEBUG
  #include "boost/nowide/iostream.hpp"
#endif

using namespace std;
using namespace boost;

Analysis *AnalysisLoader::load(int id, string analysisName, Json::Value *data)
{
    Options *options = new Options();

    JaspFiles::Path pa = Dirs::libraryDir();
    pa /= analysisName + ".json";

    JaspFiles::IFStream file(pa, ios::in);
    if (file.is_open())
    {
        Json::Value analysisDesc;
        Json::Reader parser;
        parser.parse(file, analysisDesc);
//        {
//            // Reads all the file into the file buffer.
//            string allFile;
//            const size_t buffSize = 1204;
//            char *buff = new char[buffSize + 1];
//            while (file.good() && !file.eof())
//            {
//                file.read(buff, buffSize);
//                buff[file.gcount()] = '\0';
//                allFile.append(buff);
//            }
//            delete [] buff;
//            parser.parse(allFile, analysisDesc);
//        }

        Json::Value optionsJson = analysisDesc.get("options", Json::nullValue);
        if (optionsJson != Json::nullValue)
            options->init(optionsJson);
        else
            perror("malformed analysis definition");

        bool autorun = analysisDesc.get("autorun", false).asBool();
        bool usedata = analysisDesc.get("usedata", true).asBool();
        Version version = Version(analysisDesc.get("version", "0.00").asString());

        if (data != NULL)
            options->set(*data);

        file.close();

        return new Analysis(id, analysisName, options, version, autorun, usedata);
    }

    throw runtime_error("Could not access analysis definition.");
}
