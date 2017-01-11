//
// Copyright (C) 2017 University of Amsterdam
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

#ifndef FSENTRYWIDGET_H
#define FSENTRYWIDGET_H

#include <QAbstractButton>

#include "fsentry.h"
#include "elidelabel.h"

#include <QLabel>
#include <QPixmap>

#include "common.h"


class FSEntryWidget : public QAbstractButton
{
	Q_OBJECT

public:
	explicit FSEntryWidget(const FSEntry &entry, QWidget *parent = 0);
	~FSEntryWidget();

	void setEntryInfo(const FSEntry &entry);
	void setCompact(bool compact);

	const QString &path() const;
	FSEntry::EntryType entryType() const;

	enum ClickMeans { ClickIsOpen, ClickIsSelect };

protected:
	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;
	void paintEvent(QPaintEvent *event) OVERRIDE;
	void nextCheckState() OVERRIDE;

	static QPixmap *_smallIcons;
	static QPixmap *_largeIcons;

	static const QString _uncheckedSS;
	static const QString _checkedSS;

signals:
	void selected();
	void opened();

private slots:
	void clickedHandler();
	void doubleClickHandler();

private:

	static void initIcons();

	void refresh();

	ClickMeans _clickMeans;
	QLabel *_icon;
	ElideLabel *_label;
	ElideLabel *_description;

	bool _compact;

	FSEntry _entry;
};

#endif // FILESYSTEMENTRYWIDGET_H
