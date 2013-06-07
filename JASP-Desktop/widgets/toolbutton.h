#ifndef TOOLBUTTON_H
#define TOOLBUTTON_H

#include <QPushButton>
#include <QString>

class ToolButton : public QPushButton
{
    Q_OBJECT
public:
	explicit ToolButton(QWidget *parent = 0);

signals:
    
public slots:

protected:
    virtual void paintEvent(QPaintEvent *event) override;

private:
    bool m_firstPaint = true;
    
};

#endif // TOOLBUTTON_H
