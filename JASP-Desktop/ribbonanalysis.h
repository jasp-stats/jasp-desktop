#ifndef RIBBONANALYSIS_H
#define RIBBONANALYSIS_H

#include <QWidget>

namespace Ui {
class RibbonAnalysis;
}

class RibbonAnalysis : public QWidget
{
	Q_OBJECT
	
public:
	explicit RibbonAnalysis(QWidget *parent = 0);
	~RibbonAnalysis();

signals:
	void itemSelected(QString itemName);
	
private slots:
	void itemSelected();
	void menuItemSelected();

private:
	Ui::RibbonAnalysis *ui;
};

#endif // RIBBONANALYSIS_H
