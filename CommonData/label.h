#ifndef LABEL_H
#define LABEL_H

#include <string>
#include <json/json.h>
#include "datasetbasenode.h"


class Column;
class DatabaseInterface;

/// A label
/// 
/// Label is a class that stores the value of a column if it is not a Scale (a Nominal Int, Nominal Text, or Ordinal).
/// The original value can be an integer, float or string, this is stored in a json
///
/// Internally for all non-scalar columns they are stored as ints in Column::_ints, the value of a Label corresponds to that
/// Beyond that there are some extra attributes like a description or whether it is currently allowed by the generated filter.
///
/// The order of the labels in R is determined by their order in Column::_labels,
/// and Column makes sure (_dbUpdateLabelOrder) the order is stored in the database when it is changed.
class Label : public DataSetBaseNode
{
public:	
	static const int DOUBLE_LABEL_VALUE;

								Label(Column * column);
								Label(Column * column, int value);
								Label(Column * column, const std::string & label, int value, bool filterAllows = true, const std::string & description = "", const Json::Value & originalValue = Json::nullValue, int order = -1, int id = -1);

			void				dbDelete();
			void				dbCreate();
			void				dbLoad(int labelId = -1);
			void				dbUpdate();

			Label			&	operator=(const Label &label);
			
			int					dbId()						const	{ return _dbId;				}
	const	std::string		&	description()				const	{ return _description;		}
			std::string			label()						const	{ return _label;			}
			std::string			labelDisplay()				const;
			std::string			labelIgnoreEmpty()			const;
			int					intsId()					const	{ return _intsId;			}
			bool				isEmptyValue()				const;
			int					order()						const	{ return _order;			}
			bool				filterAllows()				const	{ return _filterAllows;		}
	const	Json::Value		&	originalValue()				const	{ return _originalValue;	}
	std::pair<std::string
		,std::string>			origValDisplay()			const	{ return std::make_pair(originalValueAsString(), labelDisplay()); }

	static	std::string			originalValueAsString(const Column * column, const Json::Value & originalValue, bool fancyEmptyValue = false);
			std::string			originalValueAsString(bool fancyEmptyValue = false)		const;
			std::string			str() const;
			
			void				setIntsId(			int value);
			void				setOrder(			int order);
			void				setDbId(			int id) { _dbId = id; }
			bool				setLabel(			const std::string & label);
			bool				setOriginalValue(	const Json::Value & originalValue);
			bool				setOrigValLabel(	const Json::Value & originalValue);
			bool				setDescription(		const std::string & description);
			bool				setFilterAllows(	bool allowFilter);
			void				setInformation(Column * column, int id, int order, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue);

			Json::Value			serialize()	const;

			DatabaseInterface	& db();
	const	DatabaseInterface	& db() const;

private:

	Column		*	_column;

	Json::Value		_originalValue	= Json::nullValue;	///< Could contain integers, floats or strings. Arrays and objects are undefined.
	
	int				_dbId			= -1,	///< Database id
					_order			= -1,	///< Should correspond to its position in Column::_labels
					_intsId			= -1;	///< value of label, should always map to Column::_ints
	std::string		_label,					///< What to display in the dataview
					_description;			///< Extended information for tooltip in dataview and of course in the variableswindow
	bool			_filterAllows	= true;	///< Used in generating filters for when users disable and enable certain labels/levels
};

typedef std::vector<Label*>				Labels;
typedef std::set<Label*>				Labelset;

#endif // LABEL_H
