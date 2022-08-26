#ifndef BOUNDCONTROLCSEMTEXTAREA_H
#define BOUNDCONTROLCSEMTEXTAREA_H

#include "boundcontrollavaantextarea.h"

class BoundControlCSemTextArea : public BoundControlLavaanTextArea
{
public:
	using BoundControlLavaanTextArea::BoundControlLavaanTextArea;    
    
protected:
	static const std::string	_checkSyntaxRFunctionName;
};

#endif // BOUNDCONTROLCSEMTEXTAREA_H