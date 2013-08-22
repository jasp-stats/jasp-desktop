#ifndef HEADERVIEW_H
#define HEADERVIEW_H

#include <QHeaderView>
#include <QPixmap>

class HeaderView : public QHeaderView
{
	Q_OBJECT
public:
	explicit HeaderView(Qt::Orientation orientation, QWidget *parent = 0);

protected:
	void paintSection(QPainter *painter, const QRect &rect, int logicalIndex) const override;

private:
	//QPixmap _ordinal;
};

#endif // HEADERVIEW_H
