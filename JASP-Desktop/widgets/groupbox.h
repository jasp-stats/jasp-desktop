#ifndef GROUPBOX_H
#define GROUPBOX_H

#include <QGroupBox>

class GroupBox : public QGroupBox
{
public:
	explicit GroupBox(const QString &title, QWidget *parent = 0)
		: QGroupBox(title, parent)
	{
#ifdef __WIN32__
		setStyleSheet(styleSheet);
#elif __APPLE__
		setFlat(true);
#endif
	}

	explicit GroupBox(QWidget *parent = 0)
		: QGroupBox(parent)
	{
#ifdef __WIN32__
		setStyleSheet(styleSheet);
#elif __APPLE__
		setFlat(true);
#endif
	}

private:
#ifdef __WIN32__
	const char* styleSheet = "QGroupBox { border: none ; padding-top: 12px ; padding-left: 2px ; }";
#endif
};

#endif // GROUPBOX_H
