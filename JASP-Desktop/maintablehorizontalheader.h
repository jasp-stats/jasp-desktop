#ifndef MAINTABLEHORIZONTALHEADER_H
#define MAINTABLEHORIZONTALHEADER_H

#include <QHeaderView>
#include <QMenu>

#include "column.h"
#include "common.h"

class MainTableHorizontalHeader : public QHeaderView
{
	Q_OBJECT
public:
	explicit MainTableHorizontalHeader(QWidget *parent = 0);

signals:
	void columnTypeChanged(int columnIndex, Column::ColumnType newColumnType);

public slots:

protected:
	virtual void mousePressEvent(QMouseEvent *event) OVERRIDE;

private slots:
	void nominalSelected();
	void ordinalSelected();
	void scaleSelected();

private:
	int _columnSelected;

	QMenu *_menu;

	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

	QAction *_convertToNominal;
	QAction *_convertToOrdinal;
	QAction *_convertToScale;

};

#endif // MAINTABLEHORIZONTALHEADER_H
