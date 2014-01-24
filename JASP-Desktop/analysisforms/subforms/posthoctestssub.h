#ifndef POSTHOCTESTSSUB_H
#define POSTHOCTESTSSUB_H

#include <QWidget>

namespace Ui {
class PostHocTestsSub;
}

class PostHocTestsSub : public QWidget
{
	Q_OBJECT

public:
	explicit PostHocTestsSub(QWidget *parent = 0);
	~PostHocTestsSub();

	Ui::PostHocTestsSub *ui;

private:

};

#endif // POSTHOCTESTSSUB_H
