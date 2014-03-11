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

	options->add("sumOfSquares", new OptionList(list("type1", "type2", "type3"), "type3"));


	Options *contrastsTemplate = new Options();
	contrastsTemplate->add("variable", new OptionField());

	contrastsTemplate->add("contrast", new OptionList(list(
		"none",
		"deviation",
		"simple",
		"difference",
		"helmert",
		"repeated",
		"polynomial")));

	contrastsTemplate->add("reference", new OptionList(list("first", "last")));


	options->add("contrasts", new OptionsTable(contrastsTemplate));
	options->add("postHocTests", new OptionFields());
    options->add("marginalMeans", new OptionFields());

	options->add("ciAdjustment", new OptionList(list("LSD (none)", "Bonferroni")));
	options->add("controlCategory", new OptionList(list("first", "last"), "Last"));
	
	return options;
}

