#ifndef ReadStatImportColumn_H
#define ReadStatImportColumn_H

#include "readstat_windows_helper.h"
#include "readstat.h"
#include "../importcolumn.h"

class ReadStatImportDataSet;

///
/// Stores relevant information for a column being imported through ReadStat.
/// Tries to stay true to the datatypes as defined in the sourcefile
/// With a bit of luck it also imports the missing values per column
class ReadStatImportColumn : public ImportColumn
{
public:

                ReadStatImportColumn(readstat_variable_t * readstat_var, ReadStatImportDataSet* importDataSet, std::string name, std::string title, std::string labelsID, columnType columnType = columnType::unknown);
				~ReadStatImportColumn()							override;

			size_t						size()									const	override;
			columnType					getColumnType()							const	override	{ return _type; }
			const stringvec		&		allValuesAsStrings()					const	override;
			const stringvec		&		allLabelsAsStrings()					const	override	{ return labels();			}
			const stringset		&		allEmptyValuesAsStrings()				const	override	{ return emptyValues();		}
			bool						hasLabels()								const				{ return _labelsID != "";	}
			const std::string	&		labelsID()								const				{ return  _labelsID;		}

			void						addValue(const readstat_value_t & val);
			
			void						addLabel(const std::string		& val,	const std::string & label);

			void				addMissingValue(const std::string & missing);
			void				setType(columnType newType);


			std::string			valueAsString(size_t row)	const;
	static	std::string			readstatValueToString(const readstat_value_t & val);

			const stringvec	&	values()		const { return _values;	}
			const stringvec	&	labels()		const;
			const stringset	&	emptyValues()	const { return _missing; }


			void						tryNominalMinusText();

private:
    ReadStatImportDataSet   *   _readstatDataSet    = nullptr;
    readstat_variable_t		*	_readstatVariable   = nullptr;
	std::string					_labelsID;
	columnType					_type;
	stringvec					_values;
	stringset					_missing;
	strstrmap					_strLabels;
};

#endif // ReadStatImportColumn_H
