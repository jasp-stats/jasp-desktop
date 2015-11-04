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
