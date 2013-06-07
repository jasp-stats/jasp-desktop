#ifndef FORMANALYSISDESCRIPTIVES_H
#define FORMANALYSISDESCRIPTIVES_H

#include <QWidget>

#include "dataset.h"
#include "options.h"
#include "options/optionfields.h"

#include "analysisform.h"
#include "availablefields.h"

using namespace std;

namespace Ui {
class Descriptives;
}

class Descriptives : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit Descriptives(QWidget *parent = 0);
	~Descriptives();

private:
	Ui::Descriptives *ui;

};

#endif // FORMANALYSISDESCRIPTIVES_H
