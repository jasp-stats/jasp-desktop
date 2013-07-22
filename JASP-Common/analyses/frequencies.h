#ifndef FREQUENCIES_H
#define FREQUENCIES_H

#include "../JASP-Common/analysis.h"

#include <string>
#include <vector>

#include "../JASP-Common/lib_json/json.h"

namespace analyses
{

class Frequencies : public Analysis
{
public:
	Frequencies(int id);
    virtual void init();// override;
    virtual void run();// override;

protected:

    virtual Options *createDefaultOptions();// override;

private:

	//Json::Value initTables();
	//Json::Value initStats();
	//Json::Value initPlots();

	//vector<string> getMainFields();

	static std::string _script;
};

}

#endif // FREQUENCIES_H

