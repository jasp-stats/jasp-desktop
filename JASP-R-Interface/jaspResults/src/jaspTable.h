#pragma once
#include "jaspObject.h"
#include "jaspList.h"
#include "jaspJson.h"

struct jaspColRowCombination
{
	jaspColRowCombination(std::string name, std::string title, bool overwrite, bool removeSeparator, Json::Value colNames, Json::Value rowNames, Json::Value colOvertitles, Json::Value rowOvertitles)
		: name(name), title(title), overwrite(overwrite), removeSeparator(removeSeparator), colNames(colNames), rowNames(rowNames), colOvertitles(colOvertitles), rowOvertitles(rowOvertitles) {}

	jaspColRowCombination(Json::Value convertFromThis) { throw std::runtime_error("Not implemented");}

	std::string name, title;
	bool overwrite, removeSeparator;
	Json::Value colNames, rowNames, colOvertitles, rowOvertitles;

	std::string toString();

	Json::Value convertToJSON() { throw std::runtime_error("Not implemented"); }

};

class jaspTable : public jaspObject
{
public:
	jaspTable(std::string title = "") : jaspObject(jaspObjectType::table, title), _colNames("colNames"), _colTypes("colTypes"), _colTitles("colTitles"), _colOvertitles("colOvertitles"), _colFormats("colFormats"), _rowNames("rowNames"), _rowTitles("rowTitles") {}

	void			setColNames(Rcpp::List newNames)		{ _colNames.setRows(newNames); }
	jaspStringlist	_colNames;

	void			setColTypes(Rcpp::List newTypes)		{ _colTypes.setRows(newTypes); }
	jaspStringlist	_colTypes;

	void			setColTitles(Rcpp::List newTitles)		{ _colTitles.setRows(newTitles); }
	jaspStringlist	_colTitles;

	void			setColOvertitles(Rcpp::List newTitles)	{ _colOvertitles.setRows(newTitles); }
	jaspStringlist	_colOvertitles;

	void			setColFormats(Rcpp::List newFormats)	{ _colFormats.setRows(newFormats); }
	jaspStringlist	_colFormats;

	void			setColCombines(Rcpp::List newCombines)	{ _colCombines.setRows(newCombines); }
	jaspBoollist	_colCombines;

	void			setRowNames(Rcpp::List newNames)		{ _rowNames.setRows(newNames); }
	jaspStringlist	_rowNames;

	void			setRowTitles(Rcpp::List newTitles)		{ _rowTitles.setRows(newTitles); }
	jaspStringlist	_rowTitles;

	///Going to assume it is called like addColumInfo(name=NULL, title=NULL, type=NULL, format=NULL, combine=NULL, overTitle=NULL)
	void		addColumnInfo(Rcpp::RObject name, Rcpp::RObject title, Rcpp::RObject type, Rcpp::RObject format, Rcpp::RObject combine, Rcpp::RObject overTitle);

	///we are going to pretend that the arguments in R would be: addFootnote(message="", symbol=NULL, col_names=NULL, row_names=NULL)
	void		addFootnote(Rcpp::RObject message, Rcpp::RObject symbol, Rcpp::RObject col_names, Rcpp::RObject row_names);

	///Accepts data.frame, list, matrix or vector. If the input is one-dimensional it is assumed to be the first row, if any names are set they are copied to colNames/rowNames as far as they aren't set yet.
	void		setData(Rcpp::RObject newData);

	///Accepts data.frame, list, matrix or vector. If the input is one-dimensional it is assumed to be a single column, if two-dimensional then it will be assumed to be cols {cells/rows}, if three-dimensional or higher things probably break.
	void		addColumns(Rcpp::RObject newColumns);

	///Accepts data.frame, list, matrix or vector. If the input is one-dimensional it is assumed to be a single row, if two-dimensional then it will be assumed to be rows {cells/cols}, if three-dimensional or higher things probably break. Also fills up each column up to the maximum length one with nulls.
	void		addRows(Rcpp::RObject newRows, Rcpp::CharacterVector _rowNames);

	void		addRowsWithoutNames(Rcpp::RObject newRows) { addRows(newRows, Rcpp::CharacterVector()); }

	void		setColumn(std::string columnName, Rcpp::RObject column);

	std::string dataToString(std::string prefix) override;

	void		complete() { if(_status == "running") _status = "complete"; }

	Json::Value	metaEntry() override { return constructMetaEntry("table"); }
	Json::Value	dataEntry() override;
	std::string	toHtml()	override;

	std::string defaultColName(size_t col)			{ return "col"+ std::to_string(col); }
	std::string defaultRowName(size_t row)			{ return "row"+ std::to_string(row); }
	std::string	getRowName(size_t row)				{ return _rowNames[row] == "" ? defaultRowName(row) : _rowNames[row]; }
	std::string getColName(size_t col)				{ return _colNames[col] == "" ? defaultColName(col) : _colNames[col]; }
	std::string getColType(size_t col);

	bool		columnSpecified(size_t col)			{ return _specifiedColumns.count(getColName(col)) > 0;	}
	bool		columnSpecified(std::string col)	{ return _specifiedColumns.count(col) > 0;				}

	Json::Value	getCell(size_t col, size_t row);
	std::string	getCellFormatted(size_t col, size_t row);

	void		setExpectedSize(size_t columns, size_t rows)	{ setExpectedRows(rows); setExpectedColumns(columns);	}
	void		setExpectedRows(size_t rows)					{ _expectedRowCount = rows;								}
	void		setExpectedColumns(size_t columns)				{ _expectedColumnCount = columns;						}

private:
	std::vector<std::string>	getDisplayableColTitles(bool normalizeLengths = true, bool onlySpecifiedColumns = true);
	std::vector<std::string>	getDisplayableRowTitles(bool normalizeLengths = true);
	void						rectangularDataWithNamesToString(std::stringstream & out, std::string prefix, std::vector<std::vector<std::string>> vierkant, std::vector<std::string> sideNames, std::vector<std::string> topNames, std::map<std::string,std::string> sideOvertitles, std::map<std::string,std::string> topOvertitles);
	void						rectangularDataWithNamesToHtml(std::stringstream & out, std::vector<std::vector<std::string>> vierkant, std::vector<std::string> sideNames, std::vector<std::string> topNames, std::map<std::string,std::string> sideOvertitles, std::map<std::string,std::string> topOvertitles);


	std::map<std::string, std::string>				getOvertitlesMap();
	std::vector<std::vector<std::string>>			dataToRectangularVector(bool normalizeColLengths = false, bool normalizeRowLengths = false, bool onlySpecifiedColumns = true);
	std::vector<std::vector<std::string>>			transposeRectangularVector(const std::vector<std::vector<std::string>> & in);
	std::map<std::string, std::map<size_t, size_t>> getOvertitleRanges(std::vector<std::string> names, std::map<std::string,std::string> overtitles);

	int getDesiredColumnIndexFromNameForColumnAdding(std::string colName);
	int getDesiredColumnIndexFromNameForRowAdding(std::string colName, int previouslyAddedUnnamed);

	Json::Value	schemaJson();
	Json::Value	rowsJson();
	std::string deriveColumnType(int col);

	Json::Value convertToJSON()								override;
	void		convertFromJSON_SetFields(Json::Value in)	override;

	void	addOrSetColumnInData(std::vector<Json::Value> column, std::string colName="");
	int		pushbackToColumnInData(std::vector<Json::Value> column, std::string colName, int equalizedColumnsLength, int previouslyAddedUnnamed);

	template<int RTYPE>	void setDataFromVector(Rcpp::Vector<RTYPE> newData)
	{
		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
		extractRowNames(newData, true);

		_data.clear();
		auto cols = jaspJson::RcppVector_to_VectorJson<RTYPE>(newData);

		for(int col=0; col<cols.size(); col++)
			addOrSetColumnInData(std::vector<Json::Value>({cols[col]}), localColNames.size() > col ? localColNames[col] : "");
	}

	void setDataFromList(Rcpp::List newData)
	{
		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
		extractRowNames(newData, true);

		_data.clear();
		for(size_t col=0; col<newData.size(); col++)
			addOrSetColumnInData(jaspJson::RcppVector_to_VectorJson((Rcpp::RObject)newData[col]), localColNames.size() > col ? localColNames[col] : "");
	}

	template<int RTYPE> void setDataFromMatrix(Rcpp::Matrix<RTYPE> newData)
	{
		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
		extractRowNames(newData, true);

		std::vector<std::vector<Json::Value>> jsonMat = jaspJson::RcppMatrix_to_Vector2Json<RTYPE>(newData);

		_data.clear();
		for(size_t col=0; col<jsonMat.size(); col++)
			addOrSetColumnInData(jsonMat[col], localColNames.size() > col ? localColNames[col] : "");
	}

	void addColumnsFromList(Rcpp::List newData);

	template<int RTYPE>	void addColumnFromVector(Rcpp::Vector<RTYPE> newData)
	{
		setRowNamesWhereApplicable(extractElementOrColumnNames(newData));

		_data.push_back(jaspJson::RcppVector_to_VectorJson<RTYPE>(newData));
	}

	template<int RTYPE>	void setColumnFromVector(Rcpp::Vector<RTYPE> newData, size_t col)
	{
		setRowNamesWhereApplicable(extractElementOrColumnNames(newData));

		if(_data.size() <= col)
			_data.resize(col+1);
		_data[col] = jaspJson::RcppVector_to_VectorJson<RTYPE>(newData);
	}

	void setColumnFromList(Rcpp::List column, int colIndex);

	template<int RTYPE>	void addColumnsFromMatrix(Rcpp::Matrix<RTYPE> newData)
	{
		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
		extractRowNames(newData, true);

		std::vector<std::vector<Json::Value>> jsonMat = jaspJson::RcppMatrix_to_Vector2Json<RTYPE>(newData);

		for(size_t col=0; col<jsonMat.size(); col++)
			addOrSetColumnInData(jsonMat[col], localColNames.size() > col ? localColNames[col] : "");
	}

	template<int RTYPE>	void addRowFromVector(Rcpp::Vector<RTYPE> newData, Rcpp::CharacterVector newRowNames)
	{
		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);

		auto row = jaspJson::RcppVector_to_VectorJson<RTYPE>(newData);

		int equalizedColumnsLength = equalizeColumnsLengths();
		int previouslyAddedUnnamedCols = 0;

		for(int row=0; row<newRowNames.size(); row++)
			_rowNames[row + equalizedColumnsLength] = newRowNames[row];

		for(int col=0; col<row.size(); col++)
			previouslyAddedUnnamedCols = pushbackToColumnInData(std::vector<Json::Value>({row[col]}), localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);

	}


						static size_t lengthFromRObject(Rcpp::RObject rObj);
						static size_t lengthFromList(Rcpp::List list)				{ return list.size();	}
	template<int RTYPE> static size_t lengthFromVector(Rcpp::Vector<RTYPE> vec)		{ return vec.size();	}

	void addRowsFromList(Rcpp::List newData, Rcpp::CharacterVector newRowNames)
	{
		size_t elementLenghts = 0;
		for(int el=0; el<newData.size(); el++)
			elementLenghts = std::max(lengthFromRObject((Rcpp::RObject)newData[el]), elementLenghts);

		if(elementLenghts <= 1 && newData.size() > 1) //each entry is 1 or 0, this must be a single row with columnnames and not a set of rows with rownames..
		{
			Rcpp::List newRowList;
			auto shield = new Rcpp::Shield<Rcpp::List>(newRowList);
			newRowList.push_back(newData);
			addRowsFromList(newRowList, newRowNames);
			delete shield;

			return;
		}

		int equalizedColumnsLength = equalizeColumnsLengths();
		int previouslyAddedUnnamedCols = 0;

		std::vector<std::string> localRowNames = extractElementOrColumnNames(newData);

		for(size_t row=0; row<localRowNames.size(); row++)
			_rowNames[row + equalizedColumnsLength] = localRowNames[row];

		for(size_t row=0; row<newRowNames.size(); row++)
			_rowNames[row + equalizedColumnsLength] = newRowNames[row];


		for(size_t row=0; row<newData.size(); row++)
		{
			Rcpp::RObject rij = (Rcpp::RObject)newData[row];

			std::vector<std::string> localColNames;

			if(Rcpp::is<Rcpp::List>(rij))
				 localColNames = extractElementOrColumnNames<Rcpp::List>(Rcpp::as<Rcpp::List>(rij));

			auto jsonRij = jaspJson::RcppVector_to_VectorJson(rij);

			for(size_t col=0; col<jsonRij.size(); col++)
				previouslyAddedUnnamedCols = pushbackToColumnInData(std::vector<Json::Value>({jsonRij[col]}), localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);

		}

	}

	void addRowsFromDataFrame(Rcpp::DataFrame newData)
	{
		newData							= convertFactorsToCharacters(newData);
		int equalizedColumnsLength		= equalizeColumnsLengths();
		int previouslyAddedUnnamedCols	= 0;

		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);

		for(size_t col=0; col<newData.size(); col++)
		{
			Rcpp::RObject kolom			= (Rcpp::RObject)newData[col];
			auto jsonKolom				= jaspJson::RcppVector_to_VectorJson(kolom);
			previouslyAddedUnnamedCols	= pushbackToColumnInData(jsonKolom, localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);
		}

	}

	template<int RTYPE>	void addRowsFromMatrix(Rcpp::Matrix<RTYPE> newData, Rcpp::CharacterVector newRowNames)
	{
		std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
		// ??? something with rownames? extractRowNames(newData, true);

		int equalizedColumnsLength = equalizeColumnsLengths();
		int previouslyAddedUnnamedCols = 0;

		for(int row=0; row<newRowNames.size(); row++)
			_rowNames[row + equalizedColumnsLength] = newRowNames[row];

		auto jsonMatrix = jaspJson::RcppMatrix_to_Vector2Json<RTYPE>(newData);

		for(int col=0; col<jsonMatrix.size(); col++)
			previouslyAddedUnnamedCols = pushbackToColumnInData(std::vector<Json::Value>({jsonMatrix[col]}), localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);
	}

	void setRowNamesWhereApplicable(std::vector<std::string> rowNamesList)
	{
		for(size_t row=0; row<rowNamesList.size(); row++)
		{
			if(rowNamesList[row] != "" && (_rowNames.rowCount() <= row || _rowNames[row] == "")) //Add new rowNames or overwrite unset ones but if the user took the trouble to manually set it then just leave it I guess?
				_rowNames[row] = rowNamesList[row];
		}
	}


	///The general case might work for matrices and dataframes?
	template <typename RCPP_CLASS> std::vector<std::string> extractElementOrColumnNames(RCPP_CLASS rObj, bool setColNamesInTable=false)
	{
		std::vector<std::string> colNamesVec = jaspObject::extractElementOrColumnNames(rObj);

		for(size_t col=0; col<colNamesVec.size(); col++)
			if(setColNamesInTable && colNamesVec[col] != "" && (_colNames.rowCount() <= col || _colNames[col] == "")) //Add new columnNames or overwrite unset ones but if the user took the trouble to manually set it then just leave it I guess?
				_colNames[col] = colNamesVec[col];

		return colNamesVec;
	}

	template <typename RCPP_CLASS> std::vector<std::string> extractRowNames(RCPP_CLASS rObj, bool setRowNamesInTable=false)
	{
		Rcpp::RObject rowNamesRObject = Rcpp::rownames(rObj), rijnamesRObject = rObj.attr("row.names");
		Rcpp::CharacterVector rowNamesList;
		std::vector<std::string> rowNamesVec;

		if(!rowNamesRObject.isNULL() || !rijnamesRObject.isNULL())
		{
			rowNamesList = !rowNamesRObject.isNULL()  ? rowNamesRObject : rijnamesRObject;

			for(size_t row=0; row<rowNamesList.size(); row++)
			{
				rowNamesVec.push_back(Rcpp::as<std::string>(rowNamesList[row]));

				if(setRowNamesInTable && rowNamesList[row] != "" && (_rowNames.rowCount() <= row || _rowNames[row] == "")) //Add new rowNames or overwrite unset ones but if the user took the trouble to manually set it then just leave it I guess?
					_rowNames[row] = rowNamesList[row];
			}
		}

		return rowNamesVec;
	}

	template <typename RCPP_CLASS> void extractRowAndColumnNames(RCPP_CLASS rObj, int columnOffset = 0, int rowOffset = 0)
	{
		Rcpp::RObject colNamesRObject = Rcpp::colnames(rObj), rowNamesRObject = Rcpp::rownames(rObj), kolnamesRObject = rObj.names(), rijnamesRObject = rObj.attr("row.names");
		Rcpp::CharacterVector colNamesList, rowNamesList;

		if(!colNamesRObject.isNULL() || !kolnamesRObject.isNULL())
		{
			colNamesList = !colNamesRObject.isNULL()  ? colNamesRObject : kolnamesRObject;

			for(size_t col=0; col<colNamesList.size(); col++)
				if(colNamesList[col] != "" && (_colNames.rowCount() <= col + columnOffset || _colNames[col + columnOffset] == "")) //Add new columnNames or overwrite unset ones but if the user took the trouble to manually set it then just leave it I guess?
					_colNames[col + columnOffset] = colNamesList[col];
		}

		if(!rowNamesRObject.isNULL() || !rijnamesRObject.isNULL())
		{
			rowNamesList = !rowNamesRObject.isNULL() ? rowNamesRObject : rijnamesRObject;

			for(size_t row=0; row<rowNamesList.size(); row++)
				if(rowNamesList[row] != "" && (_rowNames.rowCount() <= row + rowOffset || _rowNames[row + rowOffset] == "")) //Add new rowNames or overwrite unset ones but if the user took the trouble to manually set it then just leave it I guess?
					_rowNames[row + rowOffset] = rowNamesList[row];
		}
	}

	int equalizeColumnsLengths();



public:
	bool					_transposeTable = false,
							_transposeWithOvertitle = false,
							_showSpecifiedColumnsOnly = false;
	std::string				_status = "running";

	std::set<std::string>	_specifiedColumns;

private:
	Json::Value								_footnotes = Json::arrayValue;
	std::vector<std::vector<Json::Value>>	_data;	//First columns, then rows.
	std::vector<jaspColRowCombination>		_colRowCombinations;
	size_t									_expectedColumnCount	= 0,
											_expectedRowCount		= 0;

};

class jaspTable_Interface : public jaspObject_Interface
{
public:
	jaspTable_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	jaspStringlist_Interface	getColNames()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colNames)		); }
	jaspStringlist_Interface	getColTypes()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colTypes)		); }
	jaspStringlist_Interface	getColTitles()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colTitles)		); }
	jaspStringlist_Interface	getColOvertitles()		{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colOvertitles)	); }
	jaspStringlist_Interface	getColFormats()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colFormats)		); }
	jaspBoollist_Interface		getColCombines()		{ return jaspBoollist_Interface(	&(((jaspTable*)myJaspObject)->_colCombines)		); }
	jaspStringlist_Interface	getRowNames()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_rowNames)		); }
	jaspStringlist_Interface	getRowTitles()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_rowTitles)		); }

	void setColNames(Rcpp::List newNames)				{ ((jaspTable*)myJaspObject)->setColNames(newNames);		}
	void setColTypes(Rcpp::List newTypes)				{ ((jaspTable*)myJaspObject)->setColTypes(newTypes);		}
	void setColTitles(Rcpp::List newTitles)				{ ((jaspTable*)myJaspObject)->setColTitles(newTitles);		}
	void setColOvertitles(Rcpp::List newTitles)			{ ((jaspTable*)myJaspObject)->setColOvertitles(newTitles);	}
	void setColFormats(Rcpp::List newFormats)			{ ((jaspTable*)myJaspObject)->setColFormats(newFormats);	}
	void setColCombines(Rcpp::List newCombines)			{ ((jaspTable*)myJaspObject)->setColCombines(newCombines);	}
	void setRowNames(Rcpp::List newNames)				{ ((jaspTable*)myJaspObject)->setRowNames(newNames);		}
	void setRowTitles(Rcpp::List newTitles)				{ ((jaspTable*)myJaspObject)->setRowTitles(newTitles);		}

	void addColumnInfo(Rcpp::RObject name, Rcpp::RObject title, Rcpp::RObject type, Rcpp::RObject format, Rcpp::RObject combine, Rcpp::RObject overtitle)	{ ((jaspTable*)myJaspObject)->addColumnInfo(name, title, type, format, combine, overtitle); }
	void addFootnote(Rcpp::RObject message, Rcpp::RObject symbol, Rcpp::RObject col_names, Rcpp::RObject row_names)											{ ((jaspTable*)myJaspObject)->addFootnote(message, symbol, col_names, row_names); }

	void setData(Rcpp::RObject newData)							{ ((jaspTable*)myJaspObject)->setData(newData);			}
	void addColumns(Rcpp::RObject newColumns)					{ ((jaspTable*)myJaspObject)->addColumns(newColumns);	}

	//void combineColumns(Rcpp::map_named_args named_args)			{ ((jaspTable*)myJaspObject)->combineColumns(named_args);	}
	//void combineRows(Rcpp::map_named_args named_args)				{ ((jaspTable*)myJaspObject)->combineRows(named_args);		}

	void addRows(Rcpp::RObject newRows, Rcpp::CharacterVector rowNames)	{ ((jaspTable*)myJaspObject)->addRows(newRows, rowNames);		}
	void addRowsWithoutNames(Rcpp::RObject newRows)						{ ((jaspTable*)myJaspObject)->addRowsWithoutNames(newRows);		}
	void setColumn(std::string columnName, Rcpp::RObject column)		{ ((jaspTable*)myJaspObject)->setColumn(columnName, column);	}

	void setExpectedSize(size_t columns, size_t rows)	{ ((jaspTable*)myJaspObject)->setExpectedSize(columns, rows);	}
	void setExpectedRows(size_t rows)					{ ((jaspTable*)myJaspObject)->setExpectedRows(rows);			}
	void setExpectedColumns(size_t columns)				{ ((jaspTable*)myJaspObject)->setExpectedColumns(columns);		}


	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, bool,			_transposeTable,				TransposeTable)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, bool,			_transposeWithOvertitle,		TransposeWithOvertitle)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, std::string,	_status,						Status)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, bool,			_showSpecifiedColumnsOnly,		ShowSpecifiedColumnsOnly)
};

RCPP_EXPOSED_CLASS_NODECL(jaspTable_Interface)
