#ifndef MINITABIMPORTER_H
#define MINITABIMPORTER_H

#include "importer.h"
#include <QCoreApplication>
#include "timers.h"


class MinitabImporter : public Importer
{
	Q_DECLARE_TR_FUNCTIONS(ExcelImporter)
public:
	MinitabImporter();
	virtual ~MinitabImporter() {}

protected:
	ImportDataSet* loadFile(const std::string &locator, std::function<void(int)> progressCallback) override;

private:
	JASPTIMER_CLASS(ExcelImporter);
};

#endif // MINITABIMPORTER_H
