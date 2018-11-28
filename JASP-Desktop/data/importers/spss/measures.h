#ifndef MEASURES_H
#define MEASURES_H

namespace spss
{

/*
 * SPSS uses this metric 'measure'  to deterime both data type
 * double / integere and the type of the number, Ornial Cardial etc.
 */
typedef enum e_measures
{
	measure_undefined = -1,
	measure_spss_unknown = 0,
	measure_nominal = 1,
	measure_ordinal = 2,
	measure_continuous = 3
} Measure;
}// End namespace spss

#endif // MEASURES_H
