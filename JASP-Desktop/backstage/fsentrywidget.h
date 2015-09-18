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
