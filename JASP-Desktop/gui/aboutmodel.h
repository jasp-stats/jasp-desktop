#ifndef ABOUTMODEL_H
#define ABOUTMODEL_H

#include <QObject>

class AboutModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY(bool		visible				READ visible			WRITE setVisible				NOTIFY visibleChanged						)
	Q_PROPERTY(QString	version				READ version											NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	buildDate			READ buildDate											NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	copyrightMessage	READ copyrightMessage									NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	downloadUrl			READ downloadUrl										NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	citation			READ citation											NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	warranty			READ warranty											NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	citationUrl			READ citationUrl										NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	commit				READ commit												NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	commitUrl			READ commitUrl											NOTIFY dummyNotifyBecauseWarNeverChanges	)
	Q_PROPERTY(QString	branch				READ branch												NOTIFY dummyNotifyBecauseWarNeverChanges	)

public:
	explicit AboutModel(QObject *parent = nullptr) : QObject(parent) {}

			bool	visible()		const;
	static	QString version();
	static	QString buildDate();
	static	QString copyrightMessage();
	static	QString downloadUrl()		{ return "https://jasp-stats.org/download/"; }
	static	QString citation();
	static	QString citationUrl()		{ return "https://jasp-stats.org/faq/how-do-i-cite-jasp/"; }
	static	QString warranty()			{ return "This program is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING <br>THE WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A <br> PARTICULAR PURPOSE."; }
	static	QString commit();
	static	QString commitUrl()			{ return "https://github.com/jasp-stats/jasp-desktop/commit/" + commit(); }
	static	QString branch();

public slots:
	void setVisible(bool visible);

signals:
	void visibleChanged(bool visible);
	void dummyNotifyBecauseWarNeverChanges(); // https://www.youtube.com/watch?v=LtWpU6BetjQ

private:
	bool _visible = false;
};

#endif // ABOUTMODEL_H
