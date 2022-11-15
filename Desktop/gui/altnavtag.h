#ifndef ALTNAVTAG_H
#define ALTNAVTAG_H

#include <QQuickItem>
#include <QObject>

class ALTNavScope;

class ALTNavTag : public QObject
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( QString	tagText		READ	getTagText		NOTIFY	tagTextChanged	);
	Q_PROPERTY( bool	active		READ	getActive		NOTIFY	activeChanged	);

public:
	explicit ALTNavTag(QQuickItem* attachee = nullptr);
	~ALTNavTag();

	void setFullTag(QString _fullTag);
	void setActive(bool _active);

	void setTagText(QString tag);

	ALTNavScope* scope = nullptr;
	QString fullTag;

signals:
	void tagTextChanged();
	void activeChanged();

private:
	bool getActive() { return active; };
	QString getTagText() { return tagText; };

private:
	QQuickItem* attachee;
	QQuickItem* tagItem;

	QString tagText = "";
	bool active;


};

#endif // ALTNAVTAG_H
