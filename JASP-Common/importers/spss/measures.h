#ifndef MEASURES_H
#define MEASURES_H

namespace spss
{

/*
 * SPSS uses this metric mewaue to deterime both data type
 * double / string and the type of the number, Ornial Cardial etc.
 */
class Measures
{
public:
	enum e_measures
	{
		string_type = -2,
		measure_undefined = -1,
		measure_spss_unknown = 0,
		measure_nominal = 1,
		measure_ordinal = 2,
		measure_continuous = 3
	};
};
}// End namespace spss

#endif // MEASURES_H
