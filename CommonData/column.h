#ifndef COLUMN_H
#define COLUMN_H

#include "datasetbasenode.h"
#include "label.h"
#include "columntype.h"
#include "utils.h"
#include <list>

class DataSet;
class Analysis;
typedef std::vector<Label*> Labels;

/// A column of data
/// 
/// Stores the integers or doubles of the current column (both might be stored in the DB but only the relevant one is loaded)
/// It can also have its own labels which are in the child _labels that mirrors the relevant entries from table Labels.
/// Relevant being that they have a link to this column.
/// 
/// It has a variety of utility functions to modify, reorder or init values in the column.
/// As well as UI support functions for modifying the labels and such.
/// 
/// It also handles storing the information of computed columns (those used to be split off)
class Column : public DataSetBaseNode
{
public:
									Column(DataSet * data, int id = -1);
									
				DatabaseInterface & db();
		const	DatabaseInterface & db() const;

			void					dbCreate(	int index);
			void					dbLoad(		int id=-1, bool getValues = true);	///< Loads *and* reloads from DB!
			void					dbLoadIndex(int index, bool getValues = true);
			void					dbUpdateComputedColumnStuff();
			void					dbDelete(bool cleanUpRest = true);
																														
			
			void					setName(			const std::string & name			);
			void					setTitle(			const std::string & title			);
			bool					setRCode(			const std::string & rCode			);
			bool					setError(			const std::string & error			);
			void					setType(			columnType			colType			);
			columnTypeChangeResult	changeType(			columnType			colType			);
			void					setCodeType(		computedColumnType	codeType		);
			void					setDescription(		const std::string & description		);
			bool					setConstructorJson(	const Json::Value & constructorJson	);
			bool					setConstructorJson(	const std::string & constructorJson	);
			void					setAnalysisId(		int					analysisId		);
			void					setInvalidated(		bool				invalidated		);
			void					setIsComputed(		bool				isComputed		);
			void					setCompColStuff(bool   invalidated, computedColumnType   codeType, const	std::string & rCode, const	std::string & error, const	Json::Value & constructorJson);
			void					setDefaultValues(enum columnType columnType = columnType::unknown);

			bool					initAsScale(			size_t colNo, std::string newName, const doublevec	& values);
			intstrmap				initAsNominalText(		size_t colNo, std::string newName, const stringvec	& values, const strstrmap & labels);
			bool					initAsNominalOrOrdinal(	size_t colNo, std::string newName, const intvec		& values,									bool is_ordinal = false);
			bool					initAsNominalOrOrdinal(	size_t colNo, std::string newName, const intvec		& values, const intstrmap &uniqueValues,	bool is_ordinal = false);

			bool					setAsScale(				const doublevec & values);
			intstrmap				setAsNominalText(		const stringvec	& values, const strstrmap & labels,			bool *	changedSomething = nullptr);
			intstrmap				setAsNominalText(		const stringvec & values,									bool *	changedSomething = nullptr) {	return setAsNominalText(values, {}, changedSomething); }
			bool					setAsNominalOrOrdinal(	const intvec	& values,									bool	is_ordinal = false);
			bool					setAsNominalOrOrdinal(	const intvec	& values, intstrmap uniqueValues,			bool	is_ordinal = false);
			
			bool					resetEmptyValues(intstrmap &emptyValuesMap);

			bool					overwriteDataWithScale(	 doublevec	scalarData);
			bool					overwriteDataWithOrdinal(intvec		ordinalData, intstrmap levels);
			bool					overwriteDataWithNominal(intvec		nominalData, intstrmap levels);
			bool					overwriteDataWithOrdinal(intvec		ordinalData);
			bool					overwriteDataWithNominal(intvec		nominalData);
			bool					overwriteDataWithNominal(stringvec	nominalData);
			
			bool					allLabelsPassFilter()	const;
			bool					hasFilter()				const;
			void					resetFilter();
			void					incRevision();
			bool					checkForUpdates();

			bool					isColumnDifferentFromStringValues(const stringvec & strVals) const;

			columnType				type()					const	{ return _type;				}
			int						id()					const	{ return _id;				}
			int						analysisId()			const	{ return _analysisId;		}
			bool					isComputed()			const	{ return _isComputed;		}
			bool					invalidated()			const	{ return _invalidated;		}
			computedColumnType		codeType()				const	{ return _codeType;			}
			const std::string	&	name()					const	{ return _name;				}
			const std::string	&	title()					const	{ return _title;			}
			const std::string	&	description()			const	{ return _description;		}
			const std::string	&	error()					const	{ return _error;			}
			const std::string	&	rCode()					const	{ return _rCode;			}
				  std::string		rCodeStripped()			const	{ return stringUtils::stripRComments(_rCode);	}
				  std::string		constructorJsonStr()	const	{ return _constructorJson.toStyledString();	}
			const Json::Value	&	constructorJson()		const	{ return _constructorJson;	}
			size_t					rowCount()				const	{ return _type == columnType::scale ? _dbls.size() : _ints.size(); }
			const intvec		&	ints()					const	{ return _ints; }
			const doublevec		&	dbls()					const	{ return _dbls; }

			void					labelsClear();
			int						labelsAdd(			int display);
			int						labelsAdd(			const std::string & display);
			int						labelsAdd(			int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order=-1, int id=-1);
			void					labelsRemoveValues(	intset valuesToRemove);
			strintmap				labelsResetValues(	int & maxValue);
			void					labelsRemoveBeyond( size_t indexToStartRemoving);

			bool					labelsSyncInts(		const intset	& dataValues);
			bool					labelsSyncIntsMap(	const intstrmap	& dataValues);
			strintmap				labelsSyncStrings(	const stringvec	& new_values, const strstrmap &new_labels, bool * changedSomething = nullptr);

			std::set<size_t>		labelsMoveRows(std::vector<size_t> rows, bool up);
			void					labelsReverse();

			std::string				operator[](size_t row); ///< Display value/label for row
			std::string				getValue(size_t row,	bool fancyEmptyValue = false) const;
			
			bool					setStringValueToRowIfItFits(size_t row, const std::string & value);
			void					setValue(					size_t row, int					value, bool writeToDB = true);
			void					setValue(					size_t row, double				value, bool writeToDB = true);
			void					setValues(								const intvec	&	values);
			void					setValues(								const doublevec	&	values);
			void					rowInsertEmptyVal(size_t row);
			void					rowDelete(size_t row);
			void					setRowCount(size_t row);

			Labels				&	labels()												{ return _labels; }
			const Labels		&	labels()										const	{ return _labels; }
			Label				*	labelByValue(	int					value)		const; ///< Might be nullptr for missing value
			Label				*	labelByDisplay(	const std::string & display)	const; ///< Might be nullptr for missing display
			Label				*	labelByRow(		int					row)		const; ///< Might be nullptr for missing
			int						labelIndex(		const Label * label)			const;




			bool					isValueEqual(size_t row, double value)				 const;
			bool					isValueEqual(size_t row, int value)					 const;
			bool					isValueEqual(size_t row, const std::string &value)	 const;

			intset					getUniqueLabelValues() const;
			
			void					beginBatchedLabelsDB();
			void					endBatchedLabelsDB(bool wasWritingBatch = true);
			bool					batchedLabel()	{ return _batchedLabel; }

			void					rememberOriginalColumnType();
			
			DataSet				*	data() const { return _data; }

			void					loadComputedColumnJsonBackwardsCompatibly(const Json::Value & fromJaspFile);
			void					invalidate()																			{ setInvalidated(true);		}
			void					validate()																				{ setInvalidated(false);	}
			void					invalidateDependents();
			bool					hasError()																	const		{ return !error().empty();	}
			void					findDependencies();
			void					setDependsOn(const stringset & columns);
			bool					dependsOn(const std::string & columnName, bool refresh = true);
			bool					iShouldBeSentAgain();
			bool					isComputedByAnalysis(size_t analysisID);

			void					checkForLoopInDependencies(std::string code);
			const	stringset	 &	dependsOnColumns(bool refresh = true);

protected:
			void					_checkForDependencyLoop(stringset foundNames, std::list<std::string> loopList);
			bool					_setAsNominalOrOrdinal(const intvec & values, bool is_ordinal);
			void					_dbUpdateLabelOrder();		///< Sets the order of the _labels to label.order and in DB
			void					_sortLabelsByOrder();		///< Sorts the labels by label.order
			std::string				_getLabelDisplayStringByValue(int key) const;
			columnTypeChangeResult	_changeColumnToNominalOrOrdinal(enum columnType newColumnType);
			columnTypeChangeResult	_changeColumnToScale();
			
			void					_convertVectorIntToDouble(intvec & intValues, doublevec & doubleValues);
			bool					_resetEmptyValuesForNominal(	intstrmap & emptyValuesMap);
			bool					_resetEmptyValuesForScale(		intstrmap & emptyValuesMap);
			bool					_resetEmptyValuesForNominalText(intstrmap & emptyValuesMap, bool tryToConvert = true);
			

private:
			DataSet		*			_data				= nullptr;
			Labels					_labels;
			columnType				_type				= columnType::unknown,
									_preEditType		= columnType::unknown;
			int						_id					= -1,
									_analysisId			= -1;
			bool					_isComputed			= false,
									_invalidated		= false,
									_batchedLabel		= false;
			computedColumnType		_codeType			= computedColumnType::unknown;
			std::string				_name,
									_title,
									_description,
									_error,
									_rCode				= "#Enter your R code here :)";
			Json::Value				_constructorJson	= Json::objectValue;
			doublevec				_dbls;
			intvec					_ints;
			stringset				_dependsOnColumns;
			std::map<int, Label*>	_labelByValueMap;
			
};

#endif // COLUMN_H
