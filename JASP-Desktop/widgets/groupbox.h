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

#ifndef GROUPBOX_H
#define GROUPBOX_H

#include <QGroupBox>
#include <QTimer>

class GroupBox : public QGroupBox
{
	Q_OBJECT

public:
	explicit GroupBox(const QString &title, QWidget *parent = 0)
		: QGroupBox(title, parent)
	{
#ifdef __WIN32__
		setStyleSheet(styleSheet);
#elif __APPLE__
		setFlat(true);
		QTimer::singleShot(0, this, SLOT(makeFlat()));
#endif
	}

	explicit GroupBox(QWidget *parent = 0)
		: QGroupBox(parent)
	{
#ifdef __WIN32__
		setStyleSheet(styleSheet);
#elif __APPLE__
		setFlat(true);
		QTimer::singleShot(0, this, SLOT(makeFlat()));
#endif
	}

private:
#ifdef __WIN32__
	const char* styleSheet = "QGroupBox { border: none ;  font: bold; margin-top: 0.5em; padding-top: .5em ; } QGroupBox::title { padding-top: -1em; padding-left: 0;}";
#endif

private slots:
#ifdef __APPLE__
	void makeFlat()
	{
		setFlat(true);
	}
#endif
};

#endif // GROUPBOX_H
