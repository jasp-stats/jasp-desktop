#ifndef FSBMCOMPUTER_H
#define FSBMCOMPUTER_H

#include "fsbmodel.h"
#include "common.h"

class FSBMComputer : public FSBModel
{
public:
	FSBMComputer();
	void refresh() OVERRIDE;

};

#endif // FSBMCOMPUTER_H
