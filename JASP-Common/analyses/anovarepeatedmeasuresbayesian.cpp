
#include "anovarepeatedmeasuresbayesian.h"

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

AnovaRepeatedMeasuresBayesian::AnovaRepeatedMeasuresBayesian(int id)
	: Analysis(id, "AnovaRepeatedMeasuresBayesian", createOptions())
{
}

Options *AnovaRepeatedMeasuresBayesian::createOptions() const
{
	Options *options = new Options();

	// main

	options->add("betweenSubjectFactors", new OptionVariables());


	// design

	vector<string> l12;
	l12.push_back("Level 1");
	l12.push_back("Level 2");

	OptionVariables *factorLevels = new OptionVariables();
	factorLevels->setValue(l12);

	Options *factorOptionsTemplate = new Options();
	factorOptionsTemplate->add("name", new OptionString("RM Factor %1"));
	factorOptionsTemplate->add("levels", factorLevels);

	OptionsTable *t = new OptionsTable(factorOptionsTemplate);

	vector<Options *> defaultValue;
	Options *defaultFirstFactor = static_cast<Options *>(factorOptionsTemplate->clone());
	OptionString *defaultFirstFactorName = static_cast<OptionString*>(defaultFirstFactor->get("name"));
	defaultFirstFactorName->setValue("RM Factor 1");
	defaultValue.push_back(defaultFirstFactor);

	t->setValue(defaultValue);

	options->add("repeatedMeasuresFactors", t);


	// rm subject cells

	options->add("repeatedMeasuresCells", new OptionVariables());


	// model

	Options *termsTemplate = new Options();
	termsTemplate->add("components", new OptionVariables());
	termsTemplate->add("isNuisance", new OptionBoolean());

	OptionsTable *modelTerms = new OptionsTable(termsTemplate);

	vector<Options*> rows;
	Options *de4ault = static_cast<Options*>(termsTemplate->clone());
	OptionVariables *name = static_cast<OptionVariables*>(de4ault->get(0));
	name->setValue("RM Factor 1");
	rows.push_back(de4ault);

	modelTerms->setValue(rows);

	options->add("modelTerms", modelTerms);

	options->add("outputEffects", new OptionBoolean());
	options->add("effectsStepwise", new OptionBoolean());

	return options;
}
