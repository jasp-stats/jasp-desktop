
#include "ancovabayesian.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionvariable.h"
#include "options/optionstable.h"

AncovaBayesian::AncovaBayesian(int id)
	: Analysis(id, "AncovaBayesian", createOptions())
{
}

Options *AncovaBayesian::createOptions() const
{
	Options *options = new Options();

	options->add("dependent", new OptionVariable());
	options->add("fixedFactors", new OptionVariables());
	options->add("randomFactors", new OptionVariables());
	options->add("covariates", new OptionVariables());

	Options *termsTemplate = new Options();
	termsTemplate->add("components", new OptionVariables());
	termsTemplate->add("isNuisance", new OptionBoolean());

	options->add("modelTerms", new OptionsTable(termsTemplate));

	options->add("outputEffects", new OptionBoolean());
	options->add("effectsStepwise", new OptionBoolean());

	options->add("bayesFactorType", new OptionList(list("BF10", "BF01", "LogBF10")));

	return options;
}
