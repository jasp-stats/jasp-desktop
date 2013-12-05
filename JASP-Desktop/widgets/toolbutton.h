#ifndef TOOLBUTTON_H
#define TOOLBUTTON_H

#include <QPushButton>
#include <QString>

#include "common.h"

class ToolButton : public QPushButton
{
    Q_OBJECT
public:
	explicit ToolButton(QWidget *parent = 0);

signals:
    
public slots:

protected:
    virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private:
    bool m_firstPaint;
    
};

#endif // TOOLBUTTON_H
