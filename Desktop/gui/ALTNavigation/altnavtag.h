#ifndef ALTNAVTAG_H
#define ALTNAVTAG_H

#include <QQuickItem>
#include <QObject>

class ALTNavScope;

class ALTNavTagBase : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY( QString		tagText			READ	getTagText		NOTIFY	tagTextChanged		);
	Q_PROPERTY( bool		active			READ	getActive		NOTIFY	activeChanged		);

public:
	 ALTNavTagBase(QQuickItem* parent = nullptr);
	~ALTNavTagBase();

	void setFullTag(QString _fullTag);
	void setActive(bool _active);

	QString fullTag;

private slots:
	void updateTagText();

signals:
	void tagTextChanged();
	void activeChanged();

private:
	bool getActive() { return active; };
	QString getTagText() { return tagText; };

private:
	QString tagText = "";
	bool active = false;

};

#endif // ALTNAVTAG_H
