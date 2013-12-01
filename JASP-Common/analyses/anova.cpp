#include "anova.h"

#include "options.h"
#include "option.h"
#include "options/optionfield.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"

using namespace std;

Anova::Anova(int id)
	: Analysis(id, "Anova")
{
}

Options *Anova::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionField("dependent"));
	options->add(new OptionFields("fixedFactors"));
	options->add(new OptionFields("randomFactors"));
	options->add(new OptionField("wlsWeights"));

	options->add(new OptionFields("modelTerms"));

	vector<string> sumOfSquares;
	sumOfSquares.push_back("type1");
	sumOfSquares.push_back("type2");
	sumOfSquares.push_back("type3");

	options->add(new OptionList("sumOfSquares", sumOfSquares, "type3"));

	return options;
}

