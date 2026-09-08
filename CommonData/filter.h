#ifndef FILTER_H
#define FILTER_H

#include "datasetbasenode.h"
#include "variableinfo.h"
#include <string>
#include <vector>
#include "utils.h"

#define DEFAULT_FILTER_JSON	"{\"formulas\":[]}"
#define DEFAULT_FILTER_GEN	"generatedFilter <- rep(TRUE, rowcount)"
#define DEFAULT_FILTER_NAME "DEFAULT_FILTER"

class DataSet;
class VariableInfo;
class FilteredData;
class VarInfoModelProxy;
class DatabaseInterface;
class LabelFilterGenerator;

///Interface to sqlite Filters table
///
/// It both stores the values of the filter, it also stores the R-filter constructor filter and errormsgs.
/// Instead of sending all the data through json we now just tell the desktop when we are finished.
/// "revision" and sqlite then make sure it gets properly synchronized in Desktop
///
/// If a filter has a name it is used by an analysis only, if not it is part of the DataSet and coupled with the GUI
/// This means the user can (when the filter is selected) dis/enable labels and they become part of this filter.
/// The same goes for drag'n'drop filter and or Rfilter
/// Perhaps later this will be done in a different way
/// (maybe the user-gui editable filters also need a name or something later, although I guess a title/description is probably better in that case)
class Filter : public DataSetBaseNode, public VariableInfoProvider
{
	Q_OBJECT
	
	friend DataSet;
	
	Q_PROPERTY( QString			name				READ nameQ											NOTIFY nameChanged				)
	Q_PROPERTY( QString			generatedFilter		READ generatedFilterQ	WRITE setGeneratedFilterQ	NOTIFY generatedFilterChanged	)
	Q_PROPERTY( QString			rFilter				READ rFilterQ			WRITE setRFilterQ			NOTIFY rFilterChanged			)
	Q_PROPERTY( QString			constructorJson		READ constructorJsonQ	WRITE setConstructorJsonQ	NOTIFY constructorJsonChanged	)
	Q_PROPERTY( QString			constructorR		READ constructorRQ		WRITE setConstructorRQ		NOTIFY constructorRChanged		)
	Q_PROPERTY( QString			statusBarText		READ statusBarText									NOTIFY statusBarTextChanged		)
	Q_PROPERTY( QString			filterErrorMsg		READ filterErrorMsgQ								NOTIFY filterErrorMsgChanged	)
	Q_PROPERTY( bool			hasFilter			READ hasFilter										NOTIFY hasFilterChanged			)
	Q_PROPERTY( QString			defaultRFilter		READ defaultRFilter									NOTIFY defaultRFilterChanged	)
	Q_PROPERTY( int				filteredRowCount	READ filteredRowCount								NOTIFY filteredRowCountChanged	)
	Q_PROPERTY( bool			invalidated			READ invalidated									NOTIFY invalidatedChanged		)
	Q_PROPERTY( VariableInfo *	varInfo				READ varInfo										CONSTANT						)

public:
			int					rowCount(		const QModelIndex &parent = QModelIndex())										const	override;
			int					columnCount(	const QModelIndex &parent = QModelIndex())										const	override;
			QVariant			data(			const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
	

	DataSet					*	data()				const { return _data;					}
	int							id()				const { return _id;						}
	const std::string		&	name()				const { return _name;					}
	bool						isDataSetFilter()	const { return _name.empty();			} ///< If the Filter has a name it is created by an analysis or something. Otherwise it represents a (possible) combination of a drag'n'drop filter, labels-filter and/or R-filter as manually entered in the GUI
	const std::string		&	rFilter()			const { return _rFilter;				}
	const std::string		&	generatedFilter()	const;
	const std::string		&	constructorJson()	const { return _constructorJson;		}
	const std::string		&	constructorR()		const { return _constructorR;			}
	bool						invalidated()		const { return _invalidated;			}
	const std::string		&	errorMsg()			const { return _errorMsg;				}
	const std::vector<bool>	&	filtered()			const { return _filtered;				}
	int							filteredRowCount()	const { return _filteredRowCount;		}

	QString						nameQ()					const;
	QString						title()					const { return _name == DEFAULT_FILTER_NAME ? QObject::tr("Default filter") : nameQ(); };
	QString						rFilterQ()				const;
	QString						constructorRQ()			const;
	QString						statusBarText()			const	{ return _statusBarText;			}
	QString						filterErrorMsgQ()		const;
	QString						generatedFilterQ()		const;
	QString						constructorJsonQ()		const;

	void						dbCreate();
	void						dbUpdate(bool writeFiltered = false);
	void						dbUpdateErrorMsg();
	bool						dbLoad();
	bool						dbLoadResultAndError();					///< Loads (updated) filtervalues from database and the (possible) error msg, returns true if an error is set
	void						dbDelete();
	void						incRevision() override;
	bool						checkForUpdates();
			
	bool						columnUsed(const QString & name) const;

	static	const QString	&	defaultRFilter();

	bool						hasFilter()				const;

	void						setRFilterQ(		const QString & newRFilter			);
	void						setConstructorRQ(	const QString & newConstructorR		);
	void						setGeneratedFilterQ(const QString & newGeneratedFilter	);
	void						setConstructorJsonQ(const QString & newconstructorJson	);
	void						setFilterErrorMsgQ(	const QString & newFilterErrorMsg	);
	void						setStatusBarText(	const QString & newStatusBarText	);
	
	void						setRFilter(			const std::string	& rFilter);
	void						setGeneratedFilter(	const std::string	& generatedFilter);
	void						setConstructorJson(	const std::string	& constructorJson);
	void						setConstructorR(	const std::string	& constructorR);
	void						setInvalidated(		bool			 	  invalidated);
	void						setErrorMsg(		const std::string	& errorMsg);
	void						setName(			const std::string	& name);
	bool						setFilterVector(	const boolvec		& filterResult);
	void						setFilterValueNoDB(	size_t	row, bool val);
	void						setRowCount(		size_t	rows);
	void						setId(				int		id)			{ _id = id; }

	stringset					columnsUsedInConstructor()	const;
	stringset					columnsUsedInRFilter()		const;

	static bool					filterNameIsFree(DataSet * dataSet, const std::string & filterName);
	void						checkFilterResults();

	void						reset();

	DatabaseInterface		&	db();
	const DatabaseInterface	&	db() const;
	
	VariableInfo			*	varInfo();
	FilteredData			*	rowFilteredData();
	VarInfoModelProxy		*	rowFilteredVarInfo();
	VarInfoModelProxy		*	rowFilteredVarInfo()	const;
	FilteredData			*	rowFilteredData()		const;
	VariableInfo			*	varInfo()				const;
	QAbstractItemModel		*	providerModel() override;
	QVariant					provideInfo(varInfoType info, const QString& name = "", int row = 0)			const	override;
	bool						absorbInfo(	varInfoType info, const QString& name,		int row, QVariant value)		override;
	
	
signals:
	void						nameChanged();
	void						rFilterChanged();
	void						filteredChanged();
	void						updateStatusBar();
	void						hasFilterChanged();
	void						invalidatedChanged();
	void						refreshAllAnalyses(Filter * f);
	void						refreshAllCompCols(Filter * f);
	void						dataSetShouldRefresh(bool doColumnsToo=true);
	void						constructorRChanged();
	void						statusBarTextChanged();
	void						filterErrorMsgChanged();
	void						defaultRFilterChanged(); //should we cll this on a language change? Or does it automatically go right cause we reset qml?
	void						generatedFilterChanged();
	void						constructorJsonChanged();
	void						filteredRowCountChanged();
	
protected:
	void						calculateFilteredRowCount();
	void						rescanForColumns();
	void						connectionCreation();
	
protected slots:
	void						datasetChanged(int dataSetId, QStringList changedColumns, QStringList missingColumns, QMap<QString, QString> changeNameColumns, bool rowCountChanged, bool);
	

private:
	Filter(DataSet * data);
	Filter(DataSet * data, const std::string & name, bool createIfMissing = true);

	DataSet					*	_data				= nullptr;
	int							_id					= -1,
								_filteredRowCount	= 0;
	std::string					_rFilter			= "",
								_generatedFilter	= "",
								_constructorJson	= "",
								_constructorR		= "",
								_errorMsg			= "",
								_name				= "";
	bool						_invalidated		= false;
	boolvec						_filtered;
	stringset					_columnsInConstructorJson,
								_columnsUsedInRFilter;
	QString						_statusBarText;
	LabelFilterGenerator	*	_labelGen			= nullptr;
	FilteredData			*	_rowFilteredData	= nullptr;
	VarInfoModelProxy		*	_rowFilteredVarInfo	= nullptr;
	VariableInfo			*	_varInfo			= nullptr;
};

typedef std::vector<Filter*>	Filters;
typedef std::set<Filter*>		FilterSet;

#endif // FILTER_H
