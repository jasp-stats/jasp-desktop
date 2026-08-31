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
#ifndef PROCESSHELPER_H
#define PROCESSHELPER_H

#include <QProcessEnvironment>

///
/// Makes sure the relevant environment variables required for an engine to function are set in a central place.
///
class ProcessHelper
{
public:

    /// When set ("1"), jaspBase saves the jaspResults RDS alongside the
    /// JSON results for every finished analysis. Required by consumers such
    /// as RoboReport that read results back from the RDS.
    static constexpr const char* kResultsRdsEnvVar	= "JASP_RESULTS_RDS";

    static QProcessEnvironment	getProcessEnvironmentForJaspEngine();
#ifdef _WIN32 
	static void					fixPATHForWindows(QProcessEnvironment & env);
#endif
	
private:
	ProcessHelper(){}
};

#endif // PROCESSHELPER_H
