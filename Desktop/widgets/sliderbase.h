#ifndef SLIDERBASE_H
#define SLIDERBASE_H

#include "analysis/jaspcontrol.h"
#include "analysis/options/optionnumber.h"
#include "analysis/options/boundcontrol.h"
#include <QObject>

class SliderBase : public JASPControl, public BoundControl
{
	Q_OBJECT
	
public:
	SliderBase(QQuickItem* parent = nullptr);
	void	bindTo(Option *option)						override;
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

#endif // SLIDERBASE_H
