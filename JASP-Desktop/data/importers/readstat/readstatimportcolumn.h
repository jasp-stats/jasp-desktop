#ifndef ReadStatImportColumn_H
#define ReadStatImportColumn_H

#include "readstat_windows_helper.h"
#include "readstat.h"
#include "../importcolumn.h"

class ReadStatImportColumn : public ImportColumn
{
public:

				ReadStatImportColumn(ImportDataSet* importDataSet, std::string name, std::string labelsID, columnType columnType = columnType::unknown);
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
	void						addLabel(const std::string		& val,	const std::string & label);
	void						addMissingValue();

	std::string					valueAsString(size_t row)	const;

	const std::vector<int>					&	ints()			const { return _ints;		}
	const std::vector<double>				&	doubles()		const { return _doubles;	}
	const std::vector<std::string>			&	strings()		const { return _strings;	}

	const std::set<int>							uniqueInts()	const { return std::set<int>(_ints.begin(), _ints.end()); }
	const std::map<int,std::string>			&	intLabels()		const { return _intLabels; }
	const std::map<std::string,std::string>	&	strLabels()		const { return _strLabels; }

private:
	std::string					_labelsID;
	columnType			_type;
	std::vector<int>			_ints;
	std::vector<double>			_doubles;
	std::vector<std::string>	_strings;

	std::map<int,			std::string>	_intLabels;
	std::map<std::string,	std::string>	_strLabels;
};

#endif // ReadStatImportColumn_H
