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
	void	bindTo(Option *option)						override;
	void	resetQMLItem(QQuickItem *item)				override;
	Option* createOption()								override;
	bool	isOptionValid(Option* option)				override;
	bool	isJsonValid(const Json::Value& optionValue) override;
	Option* boundTo()									override { return _boundTo; }

private slots:
	void sliderMovedSlot();
    
protected:
	OptionNumber*	_boundTo	= nullptr;
	double			_number		= 0;
	bool			_changing	= false;
	
private slots:
	void _changeOptionHandler();
};

#endif // BOUNDQMLSLIDER_H
