//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "mainwindow.h"
#include <QApplication>
#include <QDialog>
#include <QGridLayout>
#include <QLayout>
#include <QDebug>

#include <boost/locale.hpp>

#include "application.h"

int main(int argc, char *argv[])
{
    QCoreApplication::setOrganizationName("JASP");
    QCoreApplication::setOrganizationDomain("jasp-stats.org");
    QCoreApplication::setApplicationName(QCoreApplication::organizationName());

    QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == .

    // Set up the boost filesystem to workj with the relevant API
#ifdef __WIN32__
    {
        std::locale pathLocale = boost::locale::generator().generate("winapi");
        boost::filesystem::path::imbue(pathLocale);
    }
#endif

    Application a(argc, argv);
    return a.exec();
}
