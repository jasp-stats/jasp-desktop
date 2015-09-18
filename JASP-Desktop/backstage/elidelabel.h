#ifndef ELIDELABEL_H
#define ELIDELABEL_H

#include <QLabel>

#include "common.h"

class ElideLabel : public QLabel
{
	Q_OBJECT
public:
	explicit ElideLabel(QWidget *parent = 0);

	QSize minimumSizeHint() const OVERRIDE;
	QSize sizeHint() const OVERRIDE;

protected:
	void resizeEvent(QResizeEvent *event) OVERRIDE;

private:
	QString _originalText;
	QString _modifiedText;
};

#endif // ELIDELABEL_H
