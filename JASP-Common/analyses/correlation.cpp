#include "correlation.h"

Correlation::Correlation(int id)
	: Analysis(id, "Correlation")
{
}

Options *Correlation::createDefaultOptions()
{
	Options *options = new Options();

	return options;
}
