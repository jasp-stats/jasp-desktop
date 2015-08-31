#ifndef FSBMEXAMPLES_H
#define FSBMEXAMPLES_H

#include "fsbmodel.h"
#include "common.h"

class FSBMExamples : public FSBModel
{
	Q_OBJECT

public:
	FSBMExamples(QObject *parent = NULL);
	void refresh() OVERRIDE;
};

#endif // FSBMEXAMPLES_H
