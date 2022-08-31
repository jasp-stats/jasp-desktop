#ifndef BOUNDCONTROLCSEMTEXTAREA_H
#define BOUNDCONTROLCSEMTEXTAREA_H

#include "boundcontrollavaantextarea.h"

class BoundControlCSemTextArea : public BoundControlLavaanTextArea
{
public:
	using BoundControlLavaanTextArea::BoundControlLavaanTextArea;    

protected:
	const char * _checkSyntaxRFunctionName() override { return "jaspSem:::checkCSemModel"; }
};

#endif // BOUNDCONTROLCSEMTEXTAREA_H
