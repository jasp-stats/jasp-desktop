#ifndef HOMERIBBON_H
#define HOMERIBBON_H

#include <QWidget>

namespace Ui {
class RibbonHome;
}

class RibbonHome : public QWidget
{
    Q_OBJECT
    
public:
	explicit RibbonHome(QWidget *parent = 0);
	~RibbonHome();

signals:
    void itemSelected(QString item);

private slots:
    void frequenciesItemSelected();
    void descriptivesItemSelected();
	void anovaItemSelected();
    
private:
	Ui::RibbonHome *ui;
};

#endif // HOMERIBBON_H
