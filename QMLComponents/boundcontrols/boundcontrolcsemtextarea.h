#ifndef BOUNDCONTROLCSEMTEXTAREA_H
#define BOUNDCONTROLCSEMTEXTAREA_H

#include "boundcontrolrlangtextarea.h"

class BoundControlCSemTextArea : public BoundControlRlangTextArea
{
public:
    using BoundControlRlangTextArea::BoundControlRlangTextArea;

protected:
	const char * _checkSyntaxRFunctionName() override { return "jaspSem:::checkCSemModel"; }
};

#endif // BOUNDCONTROLCSEMTEXTAREA_H
