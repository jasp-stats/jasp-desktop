#ifndef VARIABLESWIDGET_H
#define VARIABLESWIDGET_H

#include <QWidget>

#include "dataset.h"

#include "variablespage/variablestablemodel.h"
#include "variablespage/levelstablemodel.h"

namespace Ui {
class VariablesWidget;
}

class VariablesWidget : public QWidget
{
	Q_OBJECT

public:
	explicit VariablesWidget(QWidget *parent = 0);
	~VariablesWidget();

	void setDataSet(DataSet *dataSet);
	void clearDataSet();
	void setCurrentColumn(int columnindex);

signals:
	void columnChanged(QString col);
	void resetTableView();

private slots:
	void moveUpClicked();
	void moveDownClicked();
    void reverseClicked();
	void labelDataChanged(QModelIndex m1, QModelIndex m2);

private:
	Ui::VariablesWidget *ui;
	DataSet *_dataSet;
	LevelsTableModel *_levelsTableModel;
	Column *_currentColumn;

};

#endif // VARIABLESWIDGET_H
