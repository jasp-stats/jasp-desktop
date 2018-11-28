#ifndef BOUNDQMLSLIDER_H
#define BOUNDQMLSLIDER_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optionnumber.h"
#include <QObject>

class BoundQMLSlider : public QObject, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLSlider(QQuickItem* item, AnalysisQMLForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void resetQMLItem(QQuickItem *item) OVERRIDE;
	virtual Option* createOption() OVERRIDE;
	virtual Option* boundTo() OVERRIDE { return _boundTo; }

private slots:
	void sliderMovedSlot();
    
protected:
	OptionNumber *_boundTo;
	float _number;
	static bool _changing;
	
private slots:
	void _changeOptionHandler();
};

#endif // BOUNDQMLSLIDER_H
