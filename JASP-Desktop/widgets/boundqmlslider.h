#ifndef BOUNDQMLSLIDER_H
#define BOUNDQMLSLIDER_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optionnumber.h"
#include <QObject>

class BoundQMLSlider : public QObject, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLSlider(QQuickItem* item, AnalysisForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void resetQMLItem(QQuickItem *item) OVERRIDE;
	virtual Option* createOption() OVERRIDE;
	virtual bool isOptionValid(Option* option) OVERRIDE;	
	virtual Option* boundTo() OVERRIDE { return _boundTo; }

private slots:
	void sliderMovedSlot();
    
protected:
	OptionNumber *_boundTo = nullptr;
	double _number = 0;
	bool _changing = false;
	
private slots:
	void _changeOptionHandler();
};

#endif // BOUNDQMLSLIDER_H
