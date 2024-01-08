#ifndef FILTER_H
#define FILTER_H

#include "datasetbasenode.h"
#include <string>
#include <vector>
#include "utils.h"

#define DEFAULT_FILTER		"# Add filters using R syntax here, see info button for help.\n\ngeneratedFilter # by default: pass the non-R filter(s)"
#define DEFAULT_FILTER_JSON	"{\"formulas\":[]}"
#define DEFAULT_FILTER_GEN	"generatedFilter <- rep(TRUE, rowcount)"

class DataSet;
class DatabaseInterface;

///Interface to sqlite Filters table
/// 
/// It both stores the values of the filter, it also stores the R-filter constructor filter and errormsgs.
/// Instead of sending all the data through json we now just tell the desktop when we are finished.
/// "revision" and sqlite then make sure it gets properly synchronized in Desktop
class Filter : public DataSetBaseNode
{
public:
	Filter(DataSet * data);

	DataSet					*	data()				const { return _data;					}
	int							id()				const { return _id;						}
	const std::string		&	rFilter()			const { return _rFilter;				}
	const std::string		&	generatedFilter()	const { return _generatedFilter;		}
	const std::string		&	constructorJson()	const { return _constructorJson;		}
	const std::string		&	constructorR()		const { return _constructorR;			}
	const std::string		&	errorMsg()			const { return _errorMsg;				}
	const std::vector<bool>	&	filtered()			const { return _filtered;				}
	int							filteredRowCount()	const { return _filteredRowCount;		}

	void				setRFilter(			const std::string	& rFilter)			{ _rFilter			= rFilter;			dbUpdate(); }
	void				setGeneratedFilter(	const std::string	& generatedFilter)	{ _generatedFilter	= generatedFilter;	dbUpdate(); }
	void				setConstructorJson(	const std::string	& constructorJson)	{ _constructorJson	= constructorJson;	dbUpdate(); }
	void				setConstructorR(	const std::string	& constructorR)		{ _constructorR		= constructorR;		dbUpdate(); }
	void				setErrorMsg(		const std::string	& errorMsg)			{ _errorMsg			= errorMsg;			dbUpdateErrorMsg(); }
	bool				setFilterVector(	const boolvec		& filterResult);
	void				setFilterValueNoDB(	size_t	row, bool val);
	void				setRowCount(		size_t	rows);
	void				setId(				int		id)			{ _id = id; }

	void				dbCreate();
	void				dbUpdate();
	void				dbUpdateErrorMsg();
	void				dbLoad();
	bool				dbLoadResultAndError();					///< Loads (updated) filtervalues from database and the (possible) error msg, returns true if an error is set
	void				dbDelete();
	void				incRevision() override;
	bool				checkForUpdates();

	void				reset();

	DatabaseInterface		&	db();
	const DatabaseInterface	&	db() const;
	
private:
	DataSet				*	_data				= nullptr;
	int						_id					= -1,
							_filteredRowCount	= 0;
	std::string				_rFilter			= DEFAULT_FILTER,
							_generatedFilter	= DEFAULT_FILTER_GEN,
							_constructorJson	= DEFAULT_FILTER_JSON,
							_constructorR		= "",
							_errorMsg			= "";
	std::vector<bool>		_filtered;
};

#endif // FILTER_H
