#ifndef SLIDERBASE_H
#define SLIDERBASE_H

#include "analysis/jaspcontrol.h"
#include "analysis/boundcontrolbase.h"

class SliderBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	
public:
	SliderBase(QQuickItem* parent = nullptr);

	Json::Value createJson()									override;
	bool		isJsonValid(const Json::Value& optionValue)		override;
	void		bindTo(const Json::Value& value)				override;
	void		setUp()											override;

signals:
	void		moved();

protected slots:
	void		movedSlot();
	void		_movedDelayedSlot();

protected:
	bool		_changing	= false;
};

#endif // SLIDERBASE_H
