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

	bool						isMissingValue(double d)		const { return isnan(d);				}
	bool						isMissingValue(int i)			const { return i == INT_MIN;			}
	bool						isMissingValue(std::string s)	const;

	int							missingValueInt()		const { return INT_MIN;		}
	double						missingValueDouble()	const { return NAN; }
	std::string					missingValueString()	const;

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
};

#endif // ReadStatImportColumn_H
