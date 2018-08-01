//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "utilities/application.h"

int main(int argc, char *argv[])
{
#ifdef __WIN32__
	// Temporary fix for #2322 by disabling opengl drawing on windows
	// Follow status of https://bugreports.qt.io/browse/QTBUG-61430 for 
	// future permanent fix.
	// This does slow down QML quite a bit and disables gradients
	// qputenv("QT_QUICK_BACKEND", "software");
	QCoreApplication::setAttribute(Qt::AA_UseOpenGLES); //might fix weirdlooking QML on Windows when using not-so-goo drivers? ( https://github.com/jasp-stats/jasp-desktop/issues/2669 )
#endif
	QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
	QCoreApplication::setAttribute(Qt::AA_SynthesizeTouchForUnhandledMouseEvents, false); //To avoid weird splitterbehaviour with QML and a touchscreen
	QCoreApplication::setOrganizationName("JASP");
	QCoreApplication::setOrganizationDomain("jasp-stats.org");
	QCoreApplication::setApplicationName("JASP");

	QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == .

	try
	{
		Application a(argc, argv);
		return a.exec();
	}
	catch(...)
	{
	}
	return -1;
}
