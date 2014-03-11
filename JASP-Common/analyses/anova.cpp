#include "anova.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionfield.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"
#include "options/optionstable.h"

using namespace std;

Anova::Anova(int id)
	: Analysis(id, "Anova")
{
}

Options *Anova::createDefaultOptions()
{
	Options *options = new Options();

	options->add("dependent", new OptionField());
	options->add("fixedFactors", new OptionFields());
	options->add("randomFactors", new OptionFields());
	options->add("wlsWeights", new OptionField());

	options->add("modelTerms", new OptionFields());

	vector<string> sumOfSquares;
	sumOfSquares.push_back("type1");
	sumOfSquares.push_back("type2");
	sumOfSquares.push_back("type3");

	options->add("sumOfSquares", new OptionList(sumOfSquares, "type3"));

	Options *contrastsTemplate = new Options();
	contrastsTemplate->add("variable", new OptionField());

	vector<string> contrastTypes;
	contrastTypes.push_back("none");
	contrastTypes.push_back("deviation");
	contrastTypes.push_back("simple");
	contrastTypes.push_back("difference");
	contrastTypes.push_back("helmert");
	contrastTypes.push_back("repeated");
	contrastTypes.push_back("polynomial");

	vector<string> refCategories;
	refCategories.push_back("first");
	refCategories.push_back("last");

	contrastsTemplate->add("contrast", new OptionList(contrastTypes));
	contrastsTemplate->add("reference", new OptionList(refCategories));

	options->add("contrasts", new OptionsTable(contrastsTemplate));
	options->add("postHocTests", new OptionFields());
    options->add("marginalMeans", new OptionFields());

    vector<string> ciAdjustment;
    ciAdjustment.push_back("LSD (none)");
    ciAdjustment.push_back("Bonferroni");

    options->add("ciAdjustment", new OptionList(ciAdjustment, "LSD (none)"));

    vector<string> controlCategory;
    controlCategory.push_back("Last");
    controlCategory.push_back("First");

    options->add("controlCategory", new OptionList(controlCategory, "Last"));
	
	return options;
}

