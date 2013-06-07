#ifndef JTOOLBUTTONLEFTALIGNED_H
#define JTOOLBUTTONLEFTALIGNED_H

#include <QPushButton>
#include <QString>

class RibbonButton : public QPushButton
{
    Q_OBJECT
public:
	explicit RibbonButton(QWidget *parent = 0);

signals:
    
public slots:

protected:
    virtual void paintEvent(QPaintEvent *event) override;

private:
    bool m_firstPaint = true;
    
};

#endif // JTOOLBUTTONLEFTALIGNED_H
