#ifndef JTOOLBUTTONLEFTALIGNED_H
#define JTOOLBUTTONLEFTALIGNED_H

#include <QPushButton>
#include <QString>

#include "common.h"

class RibbonButton : public QPushButton
{
    Q_OBJECT
public:
	explicit RibbonButton(QWidget *parent = 0);

signals:
    
public slots:

protected:
    virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private:
    bool _firstPaint;
    
};

#endif // JTOOLBUTTONLEFTALIGNED_H
