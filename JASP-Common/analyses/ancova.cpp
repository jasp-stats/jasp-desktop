#include "ancova.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariable.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"
#include "options/optionstable.h"

using namespace std;

Ancova::Ancova(int id)
	: Analysis(id, "Ancova", createOptions())
{
}

Options *Ancova::createOptions() const
{
	Options *options = new Options();

	// main

	options->add("dependent", new OptionVariable());
	options->add("fixedFactors", new OptionVariables());
	options->add("randomFactors", new OptionVariables());
	options->add("covariates", new OptionVariables());
	options->add("wlsWeights", new OptionVariable());


	// model

	Options *termsTemplate = new Options();
	termsTemplate->add("components", new OptionVariables());

	options->add("modelTerms", new OptionsTable(termsTemplate));
	options->add("sumOfSquares", new OptionList(list("type1", "type2", "type3"), "type3"));


	// contrasts

	Options *contrastsTemplate = new Options();
	contrastsTemplate->add("variable", new OptionVariable());

	contrastsTemplate->add("contrast", new OptionList(list(
		"none",
		"deviation",
		"simple",
		"difference",
		"helmert",
		"repeated",
		"polynomial")));

	options->add("contrasts", new OptionsTable(contrastsTemplate));


	// post hoc tests

	options->add("postHocTests/variables", new OptionVariables());
	options->add("postHocTests/bonferroni", new OptionBoolean());
    options->add("postHocTests/holm", new OptionBoolean());
    options->add("postHocTests/FDR", new OptionBoolean());
    options->add("postHocTests/hochberg", new OptionBoolean());
    options->add("postHocTests/hommel", new OptionBoolean());
    options->add("postHocTests/benjamini", new OptionBoolean());


	// options

	options->add("marginalMeans/terms", new OptionVariables());
	options->add("marginalMeans/compareMainEffects", new OptionBoolean());
	options->add("marginalMeans/ciAdjustment", new OptionList(list("LSD", "bonferroni")));

	options->add("misc/descriptives", new OptionBoolean());
	options->add("misc/effectSizeEstimates", new OptionBoolean());
	options->add("misc/homogeneityTests", new OptionBoolean());

    // plots

    options->add("horizontalAxis", new OptionVariable());
    options->add("seperateLines", new OptionVariable());
    options->add("seperatePlots", new OptionVariable());
    options->add("errorBars", new OptionBoolean());
    options->add("chartWidth", new OptionInteger(350));
    options->add("chartHeight", new OptionInteger(300));

	return options;
}

