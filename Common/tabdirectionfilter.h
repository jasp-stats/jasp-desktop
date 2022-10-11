#ifndef TABDIRECTIONFILTER_H
#define TABDIRECTIONFILTER_H

#include <QObject>

class TabDirectionFilter : public QObject
{
	Q_OBJECT
public:
	explicit TabDirectionFilter(QObject *parent = nullptr);

	enum Direction { Forward, Backward };
	Q_ENUM(Direction)

	static Direction tabDirectionForward;

private:
	bool eventFilter(QObject *object, QEvent *event) override;


signals:

};

#endif // TABDIRECTIONFILTER_H
