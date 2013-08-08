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

protected:

	virtual std::string rScript(); // override
    virtual Options *createDefaultOptions();// override;

private:

	static std::string _script;

};

}

#endif // FREQUENCIES_H

