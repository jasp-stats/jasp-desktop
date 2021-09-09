#ifndef ReadStatImportColumn_H
#define ReadStatImportColumn_H

#include "readstat_windows_helper.h"
#include "readstat.h"
#include "../importcolumn.h"

class ReadStatImportColumn : public ImportColumn
{
public:

				ReadStatImportColumn(readstat_variable_t * readstat_var, ImportDataSet* importDataSet, std::string name, std::string labelsID, columnType columnType = columnType::unknown);
				~ReadStatImportColumn()							override;

	size_t						size()									const	override;
	columnType					getColumnType()							const				{ return _type; }
	std::vector<std::string>	allValuesAsStrings()					const	override;
	bool						hasLabels()								const				{ return _labelsID != ""; }
	const std::string &			labelsID()								const				{ return  _labelsID;	}

	void						addValue(const readstat_value_t & val);
	void						addValue(const double			& val);
	void						addValue(const int				& val);
	void						addValue(const std::string		& val);

	void						addLabel(const int				& val,	const std::string & label);
	void						addLabel(const double			& val,	const std::string & label);
	void						addLabel(const std::string		& val,	const std::string & label);

	static bool						isMissingValue(double d)		{ return isnan(d);				}
	static bool						isMissingValue(int i)			{ return i == std::numeric_limits<int>::lowest();			}
	static bool						isMissingValue(std::string s);

	static int							missingValueInt()			{ return std::numeric_limits<int>::lowest();		}
	static double						missingValueDouble()		{ return NAN; }
	static std::string					missingValueString();

	void						addMissingValue();
	void						addLeadingMissingValues();
	void						setType(columnType newType);
	bool						canConvertToType(columnType newType);

			std::string			valueAsString(size_t row)	const;
	static	std::string			readstatValueToString(const readstat_value_t & val);

	const std::vector<int>					&	ints()			const { return _ints;		}
	const std::vector<double>				&	doubles()		const { return _doubles;	}
	const std::vector<std::string>			&	strings()		const { return _strings;	}

	const std::map<int,std::string>			&	intLabels()		const { return _intLabels; }
	const std::map<std::string,std::string>	&	strLabels()		const { return _strLabels; }

	void						tryNominalMinusText();

private:
	readstat_variable_t		*	_readstatVariable = nullptr;
	std::string					_labelsID;
	columnType					_type;
	std::vector<int>			_ints;
	std::vector<double>			_doubles;
	std::vector<std::string>	_strings;
	size_t						_leadingMissingValues = 0;

	std::map<int,			std::string>	_intLabels;
	std::map<std::string,	std::string>	_strLabels;
	std::set<std::string>					_loggedMissing;
};

#endif // ReadStatImportColumn_H
