#ifndef COLUMN_H
#define COLUMN_H

#include "datasetbasenode.h"
#include "label.h"
#include "columntype.h"
#include "utils.h"
#include <list>
#include "emptyvalues.h"
#include <cmath>

class DataSet;
class Analysis;

/// A column of data
/// 
/// It can have 3 columnTypes, scalar, ordinal or nominal (nominalText is a relict of the past).
/// 
/// The data itself is stored in a hybrid fashion, where any values are stored in double (and thus int)-format in _dbls.
/// Anything with a label on it will have a Label-object in Column and the "intsId" of this label is entered in the corresponding _ints row.
/// If the originalValue of that label is convertible to double/int it will *also* be stored in _dbls. (Which is different from how <0.19 JASPs did it)
/// 
/// If no label exists _ints simply contains Label::NO_LABEL (-1) and it tells JASP that _dbl should be used.
/// We do want users to be able to edit them, or to set "filter allows" or something on it.
/// To this end labelsNonEmptyCount() can be called to get the total of "labels" a column has.
/// The shown labels are stored in a temporary internal representation (stringvec).
/// 
/// What this means is that a column could have "labels" visible in the label-editor, but _labels.size() == 0!
/// This is great because changing a column with 1million unique doubles doesnt need 1 million new labels, but no operations at all.
/// Pretty nice
/// 
/// However, this does mean that things like reversing labels or disabling filterAllows on a labels requires creating all those labels.
/// This is done with Column::replaceDoubleWithLabel(*) and co.
/// 
/// The logic of getting "labels" or "values" from a column is now in Column::dataAsRLevels Column::dataAsRDoubles instead of in rbridge.
/// Although the conversion to whatever R expects is still done there (rbridge_readDataSet). But perhaps it could be moved further to the jaspRcpp side of things.
/// 
/// It has a variety of utility functions to modify, reorder or init values in the column.
/// As well as UI support functions for modifying the labels and such.
/// 
/// It also handles storing the information of computed columns (those used to be split off)
class Column : public DataSetBaseNode
{
	Q_OBJECT
	
	Q_PROPERTY(QString				name				READ nameQ					WRITE setNameQ					NOTIFY nameChanged				)
	Q_PROPERTY(QString				title				READ titleQ					WRITE setTitleQ					NOTIFY titleChanged				)
	Q_PROPERTY(QString				description			READ descriptionQ			WRITE setDescriptionQ			NOTIFY descriptionChanged		)
	Q_PROPERTY(QString				computeFilter		READ computeFilterQ			WRITE setComputeFilterQ			NOTIFY computeFilterChanged		)
	Q_PROPERTY(bool					autoSortByValue		READ autoSortByValue								NOTIFY autoSortByValueChanged	)
	Q_PROPERTY(QString				error				READ errorQ					WRITE setErrorQ					NOTIFY errorChanged				)
	Q_PROPERTY(QString				rCode				READ rCodeQ					WRITE setRCodeQ					NOTIFY rCodeChanged				)
	Q_PROPERTY(columnType			colType				READ type					WRITE setType					NOTIFY columnTypeChanged		)
	Q_PROPERTY(computedColumnType	codeType			READ codeType				WRITE setCodeType				NOTIFY codeTypeChanged			)
	Q_PROPERTY(bool					hasLabels			READ hasLabels				WRITE setHasLabels				NOTIFY hasLabelsChanged			)
	Q_PROPERTY(QString				constructorJson		READ constructorJsonQ		WRITE setConstructorJsonQ		NOTIFY constructorJsonChanged	)
	// Emit signals also in refresh
	
	friend DatabaseInterface;
public:
	typedef std::map<std::pair<std::string, std::string>, Label*>	LabelByStrStr;
	typedef std::map<std::string, Labelset>							LabelsByStr;
	friend DataSet;

protected:
									Column(DataSet * data, int id = -1);	///< Dont use directly! Use DataSet::_createColumn

public:
									~Column();			
									
				DatabaseInterface & db();
		const	DatabaseInterface & db() const;

			void					dbCreate(	int index);
			void					dbLoad(		int id=-1, bool getValues = true);	///< Loads *and* reloads from DB!
			void					dbLoadOldIndex(	int index);		///< Loads pre ~0.96.1 data (with both DBL and INT cols)
			void					dbLoadIndex(int index, bool getValues = true);
			void					dbUpdateComputedColumnStuff();
			void					dbUpdateValues();
			void					dbDelete(bool cleanUpRest = true);
			
			int						rowCount(		const QModelIndex &parent = QModelIndex())										const	override;
			int						columnCount(	const QModelIndex &parent = QModelIndex())										const	override;
			QVariant				headerData(		int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const	override;
			QVariant				data(			const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
			bool					setData(		const QModelIndex &index, const QVariant &value, int role)								override;
			
			void					refresh(bool doDataChanged = true);
			QList<QVariant>			getColumnValuesAsDoubleList()	const;

			bool					setLabelDescription(int labelRow, const QString &	newDescription	);
			bool					setLabelDisplay(	int labelRow, const QString &	newLabel		);
			bool					setLabelValue(		int labelRow, const QString &	newLabelValue	);
			bool					setLabelAllowFilter(int labelRow, bool				newAllowValue	);

			std::string				generateLabelFilter() const;
																															
			void					setNameManually(	const QString	  & name			);
			bool					setName(			const std::string & name			);
			bool					setNameQ(			const QString	  & name			);
			void					setTitle(			const std::string & title			);
			void					setTitleQ(			const QString	  & title			);
			void					setHasLabels(		bool				haveLabels		);
			bool					setRCode(			const std::string & rCode			);
			bool					setRCodeQ(			const QString	  & rCode			);
			bool					setError(			const std::string & error			);
			bool					setErrorQ(			const QString	    error			);
			void					setType(			columnType			colType			);
			columnTypeChangeResult	changeType(			columnType			colType			);
			void					setCodeType(		computedColumnType	codeType		);
			void					setDescription(		const std::string & description		);
			void					setDescriptionQ(	const QString	  & description		);
			void					setComputeFilter(	const std::string & filter = ""		);
			void					setComputeFilterQ(	const QString		& filter			);
			bool					setConstructorJson(	const Json::Value & constructorJson	);
			bool					setConstructorJson(	const std::string & constructorJson	);
			bool					setConstructorJsonQ(const QString	  & constructorJson	);
			void					setAutoSortByValue(	bool				sort			);
			void					setAnalysisId(		int					analysisId		);
			void					setIndex(			int					index			);
			void					setInvalidated(		bool				invalidated		);
			void					setCompColStuff(	bool				invalidated, computedColumnType   codeType, const	std::string & rCode, const	std::string & error, const	Json::Value & constructorJson);
			void					setDefaultValues(	enum columnType		columnType = columnType::unknown, bool emitValues = true);
			void					setDropLevels(		dropLevelsType		dropEm);

			bool					setAsNominalOrOrdinal(	const intvec	& values,									bool	is_ordinal = false);
			bool					setAsNominalOrOrdinal(	const intvec	& values, intstrmap uniqueValues,			bool	is_ordinal = false);

			bool					initFromLookups(const std::string & newName, size_t rows, const std::function<std::string(size_t)> valueLookup, const std::function<std::string(size_t)> labelLookup, const std::string & title, columnType desiredType, const stringset & emptyValues, int threshold, bool orderLabelsByValue, bool useLocale = true); ///< useLocale false: numbers in valueLookup are written the way C writes them
			bool					overwriteDataAndType(	stringvec		data, columnType colType, bool computed);
			void					labelsToNoLabels(bool signalOthers = true);
			void					noLabelsToLabels();
			
			bool					allLabelsPassFilter()	const;
			bool					hasLabelFilter()				const;
			void					resetFilter();
			void					incRevision() override;
			bool					checkForUpdates();
			void					addLabelManually(QString value, QString label);
			void					deleteLabelManually(int labelIndex);

			bool					isColumnDifferentFromStringLookUps(const std::string & title, size_t rows,	const std::function<std::string(size_t)> valueLookup, const std::function<bool(size_t, double &)> numberLookup, const std::function<std::string(size_t)> labelLookup, const stringset & strEmptyVals) const; ///< valueLookup as the data shows it, numberLookup the number an import reads there (false for text)

			columnType				type()					const	{ return _type;				}
			int						id()					const	{ return _id;				}
			int						analysisId()			const	{ return _analysisId;		}
Q_INVOKABLE	bool					isComputed()			const	{ return _codeType != computedColumnType::notComputed && _codeType != computedColumnType::analysisNotComputed;	}
Q_INVOKABLE	bool					isComputedRCode()		const	{ return _codeType == computedColumnType::rCode;	}
			dropLevelsType			dropLevels()			const	{ return _dropLevels;		}
			bool					shouldDropLevels()		const	{ return _dropLevels != dropLevelsType::keep; }
			bool					invalidated()			const	{ return _invalidated;		}
			bool					autoSortByValue()		const	{ return _autoSortByValue;	}
			bool					hasLabels()				const	{ return _hasLabels;		}
			computedColumnType		codeType()				const	{ return _codeType;			}
			const std::string	&	name()					const	{ return _name;				}
			const QString			nameQ()					const;
			const QString			titleQ()				const;
			const QString			rCodeQ()				const;
			const QString			descriptionQ()			const;			
			const std::string	&	title()					const	{ return _title.empty() ? _name : _title;	}
			const std::string	&	error()					const	{ return _error;			}
			const QString			errorQ()				const;
			const std::string	&	rCode()					const	{ return _rCode;			}
			const std::string	&	description()			const	{ return _description;		}
			const std::string		computeFilter()			const	{ return _computeFilter.empty() ? "DEFAULT_FILTER" : _computeFilter;		}
			QString					computeFilterQ()		const;	
				  std::string		rCodeStripped()			const	{ return stringUtils::stripRComments(_rCode);	}
				  std::string		constructorJsonStr()	const	{ return _constructorJson.toStyledString();	}
				  QString			constructorJsonQ()		const;
			const Json::Value	&	constructorJson()		const	{ return _constructorJson;	}
			const intvec		&	ints()					const	{ return _ints; }
			const doublevec		&	dbls()					const	{ return _dbls; }
			bool					numberAt(size_t row, double & number)	const;	///< The number row holds, false when it holds text or nothing
			const stringvec		&	strs()					const	{ return _strs;	}
			
			void					labelsClear(bool doIncRevision=true);
			int						labelsAdd(			int					display);
			int						labelsAdd(			double				display);
			int						labelsAdd(			const std::string & display);
			int						labelsAdd(			const std::string &	display, const std::string & value);
			int						labelsAdd(			const std::string & display, const std::string & description, const Json::Value & originalValue);
			int						labelsAdd(			int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order=-1, int id=-1);
			void					labelsRemove(		int labelIndex);
			int						labelsSet(int lbId,	int value, const std::string & display, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order=-1, int id=-1);
			void					labelsRemoveByIntsId(	intset valuesToRemove, bool updateOrder = true);
			void					labelsShrinkOnlyToSize( size_t highestToKeep);
			
			int						nonFilteredNumericsCount();
			int						nonFilteredNumericsCount()	const;
            const stringvec		&	nonFilteredLevels();
			const stringvec		&	nonFilteredLevels() const;
			void					nonFilteredCountersReset(bool updateLabelIndexes = true);

			std::set<qsizetype>		labelsMoveRows(std::vector<qsizetype> rows, bool up);
			void					labelsReverse();
			void					valuesReverse();
			void					labelsOrderByValue(bool doDbUpdateEtc=true);

			std::string				operator[](				size_t row); ///< Display value/label for row
			std::string				getValue(				size_t row,	bool fancyEmptyValue = false, bool ignoreEmptyValue = false, bool sepas = true, columnType asType = columnType::unknown)	const; ///< Returns the ("original") value. Basically whatever the user would like to see as value. Stored internally as json
			std::string				getDisplay(				size_t row,	bool fancyEmptyValue = true, bool sepas = true)									const;
			std::string				getShadow(				size_t row,	bool fancyEmptyValue = true, bool sepas = true)									const;
			std::string				getLabel(				size_t row,	bool fancyEmptyValue = false, bool ignoreEmptyValue = false, bool sepas = true)	const;
			std::string				getValueIndexNonEmpty(	size_t row,	bool fancyEmptyValue = false, bool ignoreEmptyValue = false, bool sepas = true, columnType asType = columnType::unknown)	const; ///< Returns the ("original") value. Basically whatever the user would like to see as value. Stored internally as json
			std::string				getDisplayIndexNonEmpty(size_t row,	bool fancyEmptyValue = true, bool sepas = true)									const;
			std::string				getShadowIndexNonEmpty(	size_t row,	bool fancyEmptyValue = true, bool sepas = true)									const;
			std::string				getLabelIndexNonEmpty(	size_t row,	bool fancyEmptyValue = false, bool ignoreEmptyValue = false)	const;
			
			stringvec				valuesAsStrings()																						const;
			stringvec				labelsAsStrings()																						const;
			stringvec				nonEmptyLevelsStrings()																					const;
			stringvec				displaysAsStrings()																						const;
			stringvec				dataAsRLevels(intvec & values, const boolvec & filter)			; ///< values is output! If filter is of different length than the data an error is thrown, if length is zero it is ignored. useLabels indicates whether the levels will be based on the label or on the value as specified in the label editor.
			doublevec				dataAsRDoubles(const boolvec & filter)													const; ///< If filter is of different length than the data an error is thrown, if length is zero it is ignored

			void					labelValueChanged(		Label * label,	const Json::Value & previousOriginal); ///< Pass NaN for non-convertible values
			void					labelDisplayChanged(	Label * label,	const std::string & previousDisplay);
			void					labelValDisplayChanged(	Label * label,	const std::string & previousDisplay,	const Json::Value & previousOriginal);
			
			bool					setStringValue(				size_t row, const std::string & value, const std::string & label = "", bool writeToDB = true); ///< Does two things, if label=="" it will handle user input, as value or label depending on columnType. Otherwise it will simply try to use userEntered as a value. But this will trigger the setting of type
			bool					setValue(					size_t row,		  std::string   value, const std::string & label,	bool writeToDB = true, bool useLocale = true);
			bool					setValue(					size_t row, int					value,								bool writeToDB = true);
			bool					setValue(					size_t row, double valueDbl,  const std::string & valueStr,			bool writeToDB = true);
			columnType				setValues(				const stringvec &	values, const stringvec &	labels, int thresholdScale, bool * changedSomething = nullptr, bool useLocale = true, bool determineWhetherOneWantsLabels = false); ///< Returns what would be the most sensible columntype
			columnType				setValues(size_t rows,	const std::function<std::string(size_t)> valueLookup, const std::function<std::string(size_t)> labelLookup, int thresholdScale, bool * changedSomething = nullptr, bool useLocale = true, bool determineWhetherOneWantsLabels = false); ///< Returns what would be the most sensible columntype
			
			bool					setDescriptions(	strstrmap labelToDescriptionMap); ///<Returns any changes
			void					rowInsertEmptyVal(size_t row);
			void					rowDelete(size_t row);
			void					setRowCount(size_t row);

			Labels				&	labels()																						{ return _labels; }
			const Labels		&	labels()																				const	{ return _labels; }
			size_t					labelsNonEmptyCount()																	const;
			void					labelsMergeDuplicateInto(Label * label);
			bool					labelsRemoveOrphans();
			Labelset				labelsByDisplay(		const std::string	&	display)								const; ///< SLOW! Might be nullptr for missing label
			Labelset				labelsByValue(			const std::string	&	value)									const; ///< SLOW!
			int						labelIndexNonEmpty(		Label				*	label)									const;
			Label				*	labelByRow(				int						row)									const; ///< 
			Label				*	labelByValue(			const std::string	&	value)									const; ///< Might be nullptr for missing label, returns the first of labelsByValue
			Label				*	labelByIntsId(			int						intsId)									const; ///< Might be nullptr for missing label
			Label				*	labelByDisplay(			const std::string	&	display)								const; ///< Might be nullptr for missing label, returns the first of labelsByDisplay
			Label				*	labelByIndexNonEmpty(	int						index)									const;
			Label				*	labelByValueAndDisplay(	const std::string	&	value, const std::string &	label)		const; ///< Might be nullptr for missing label, assumes you ran labelsMergeDuplicates before
			void					labelsHandleAutoSort(	bool					doDbUpdateEtc = true);

			bool					isValueEqual(size_t row, double value)				 const;
			bool					isValueEqual(size_t row, int value)					 const;
			bool					isValueEqual(size_t row, const std::string &value)	 const;

			intset					getUniqueLabelValues() const;
			
			void					beginBatchedLabelsDB();
			void					endBatchedLabelsDB(bool wasWritingBatch = true);
			bool					batchedLabelDepth()	{ return _batchedLabelDepth; }
			
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
			void					checkForDependentColumnsToBeSent(bool refreshMe = false);
			Json::Value				serialize()																const;
			Json::Value				serializeLabels(bool forCompare = false)								const;
			Json::Value				jsonForCompare()														const;
			void					deserialize(				const Json::Value & info);
			void					deserializeLabelsForCopy(	const Json::Value & info);
			void					deserializeLabelsForRevert(	const Json::Value & info);
			std::string				getUniqueName(const std::string& name)									const;
			std::string				doubleToDisplayString(	double dbl, bool fancyEmptyValue = true, bool ignoreEmptyValues = false, bool sepas=true)	const; ///< fancyEmptyValue is the user-settable empty value label, for saving to csv this might be less practical though, so turn it off
			stringvec				previewTransform(columnType transformType);
			
			bool					hasCustomEmptyValues()													const;
	const   EmptyValues    		*	emptyValues()															const { return _emptyValues; }
			void					setHasCustomEmptyValues(		bool hasCustom);
			bool					setCustomEmptyValues(			const stringset		& customEmptyValues); ///<returns whether there was a change
			bool					isEmptyValue(					const std::string	& val)															const;
			bool					isEmptyValue(					const double		  val)															const;
			
			size_t					getMaximumWidthInCharactersIncludingShadow();
			size_t					getMaximumWidthInCharacters(bool shortenAndFancyEmptyValue, bool valuesPlease, size_t	extraPad	= 4); ///< Tries to take into consideration that utf-8 can have more characters than codepoints and compensates for it
			columnType				resetValues(int thresholdScale); ///< "Reimport" the values it already has with a possibly different threshold of values 
			stringset				mergeOldMissingDataMap(const Json::Value & missingData); ///< <0.19 JASP collected the removed empty values values in a map in a json object... We need to be able to read at least 0.18.3 so here this function that absorbs such a map and adds any required labels. It does not add the empty values itself though!

	static	void					setAutoSortByValuesByDefault(bool autoSort);
	static	bool					autoSortByValuesByDefault();
			void					resetFilterAllows();
			int						filteredOut() const;
			void					tryAndRunComputedColumn();
Q_INVOKABLE	void					showAnalysisForm();
	static	QString					columnTypeFriendlyName(computedColumnType compColT);
			
protected:
			boolvec					getFilterAllows() const;
			Label *					connectNewLabel(Label *newLabel);

			doublevec				valuesNumericOrdered();			
			std::map<Label*,size_t> valuesAlphabeticalOffsets();
			
			bool					areLoopDependenciesOk(const std::string &code);
			void					_checkForDependencyLoop(stringset foundNames, std::list<std::string> loopList);
			void					_dbUpdateLabelOrder(bool noIncRevisionWhenBatchedPlease = false);		///< Sets the order of the _labels to label.order and in DB
			intintmap				_updateNonEmptyIndexesAndLabelOrder();
			void					_sortLabelsByOrder();		///< Sorts the labels by label.order
			std::string				_getLabelDisplayStringByValue(int key, bool ignoreEmptyValue = false) const;
			columnTypeChangeResult	_changeColumnToNominalOrOrdinal(enum columnType newColumnType);
			columnTypeChangeResult	_changeColumnToScale();
			void					_convertVectorIntToDouble(intvec & intValues, doublevec & doubleValues);
			void					_resetLabelValueMap();

			int						_labelMapIt(Label *label);
			void					_labelMapUpdates(Label *label, const std::string & previousDisplay, const std::string & previousOriginal);
			void					_handleWidthChangeWithoutLabels();

signals:
			void					constructorJsonChanged();
			void					manualEditMade();
			void					dataSetShouldRefresh(bool doColumnsToo=true);
			void					columnChanged(Column * c);
			void					labelsReordered(	const Column * column);
			void					labelFilterChanged();
			void					showWarning(						QString title, QString msg);
			void					labelChanged(		const Column * column, QString originalLabel, QString newLabel);
			void					nameChanged();
			void					titleChanged();
			void					rCodeChanged();
			void					hasLabelsChanged();
			void					descriptionChanged();
			void					codeTypeChanged();
			void					columnTypeChanged();
			void					errorChanged();
			void					computeFilterChanged();
			void					autoSortByValueChanged();
			
			

private:
			DataSet			* const	_data;
			EmptyValues		* const	_emptyValues;
			Labels					_labels;
			columnType				_type						= columnType::unknown;
			int						_id							= -1,
									_analysisId					= -1,	// Actually initialized in DatabaseInterface::columnInsert
									_highestIntsId				= -1;
            stringvec				_nonFilteredLevels;
			int						_nonFilteredNumericsCount	= -1;
			bool					_invalidated				= false,
									_autoSortByValue,
									_hasShadows					= false,
									_hasLabels					= false;
			dropLevelsType			_dropLevels					= dropLevelsType::noChoice;
			computedColumnType		_codeType					= computedColumnType::notComputed;
			std::string				_name,
									_title,
									_description,
									_error,
									_rCode,
									_computeFilter				= "";
			Json::Value				_constructorJson			= Json::objectValue;
			intvec					_ints; ///For when there are labels
			stringvec				_strs; ///For when there are no labels
			doublevec				_dbls; ///For when there are no labels
			stringset				_dependsOnColumns;
			std::map<int, Label*>	_labelByIntsIdMap,
									_labelByNonEmptyIndex;
			std::map<Label*, int>	_labelNonEmptyIndexByLabel;
			LabelByStrStr			_labelByValDis;
			LabelsByStr				_labelsByValue,
									_labelsByDisplay;
			int						_batchedLabelDepth			= 0,
									_maxWidthLabel				= -1,
									_maxWidthValue				= -1;
	static	bool					_autoSortByValuesByDefault;
			
			

};

typedef std::vector<Column*>	Columns;
typedef std::set<Column*>		ColumnSet;

#endif // COLUMN_H
