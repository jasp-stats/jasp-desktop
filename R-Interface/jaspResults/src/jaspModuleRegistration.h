#include <Rcpp.h>
#include "jaspResults.h"

JASP_OBJECT_CREATOR(jaspHtml)
JASP_OBJECT_CREATOR(jaspPlot)
JASP_OBJECT_CREATOR(jaspTable)
JASP_OBJECT_CREATOR(jaspState)
JASP_OBJECT_CREATOR(jaspColumn)
JASP_OBJECT_CREATOR(jaspContainer)
JASP_OBJECT_CREATOR(jaspQmlSource)
JASP_OBJECT_CREATOR_ARG(jaspResults, oldState)

RCPP_MODULE(jaspResults)
{
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspPlot);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspHtml);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspTable);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspState);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspColumn);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspResults);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspContainer);
	JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(jaspQmlSource);


	Rcpp::function("cpp_startProgressbar",	jaspResults::staticStartProgressbar);
	Rcpp::function("cpp_progressbarTick",	jaspResults::staticProgressbarTick);

	Rcpp::function("destroyAllAllocatedObjects", jaspObject::destroyAllAllocatedObjects);
	Rcpp::class_<jaspObject_Interface>("jaspObject")

		.method("print",							&jaspObject_Interface::print,											"Prints the contents of the jaspObject")
		.method("toHtml",							&jaspObject_Interface::toHtml,											"gives a string with the contents of the jaspObject nicely formatted as html")
		.method("printHtml",						&jaspObject_Interface::printHtml,										"Prints the contents of the jaspObject nicely formatted as html")

		.method("addMessage",						&jaspObject_Interface::addMessage,										"Add a message to this object")
		.method("addCitation",						&jaspObject_Interface::addCitation,										"Add a citation to this object")

		.property("title",							&jaspObject_Interface::getTitle,	&jaspObject_Interface::setTitle,	"Set the title of this object")
		.property("info",							&jaspObject_Interface::getInfo,		&jaspObject_Interface::setInfo,		"Set info aka help MD for this object")
		.property("position",						&jaspObject_Interface::getPosition,	&jaspObject_Interface::setPosition,	"Set the position of this object in it's container. By default this is at the end in the order of adding. You can specify any other value, they do not need to be next to each other or unique. The rule is: lower values (including negative) are higher in the container and when multiple objects in a container have the same position-value order is derived from adding-order.")
		.property("type",							&jaspObject_Interface::type,											"The type of this jaspObject as a string, something like: container, table, plot, json, list, results, html, state")

		.method("setOptionMustBeDependency",		&jaspObject_Interface::setOptionMustBeDependency,						"Specifies an option and it's required value, if the analysis is restarted and this option is no longer defined (like that) it will automatically destroy the object. Otherwise it will keep it.")
		.method("setOptionMustContainDependency",	&jaspObject_Interface::setOptionMustContainDependency,					"Specifies an option that should define an array and a required value that should be in it, if the analysis is restarted and this option is no longer defined or no longer contains the specified value it will automatically destroy the object. Otherwise it will keep it.")
		.method("dependOnOptions",					&jaspObject_Interface::dependOnOptions,									"Will make the object depend on the current values of the options specified in the charactervector.")
		.method("copyDependenciesFromJaspObject",	&jaspObject_Interface::copyDependenciesFromJaspObject,					"Will make the object depend on whatever the other jaspObject depends.")
  		.method("getError",							&jaspObject_Interface::getError, 										"Get the error status of this object.")
    	.method("setError",							&jaspObject_Interface::setError, 										"Set an error message on this object that which be shown in JASP. Errors set on jaspContainers or jaspResults are propagated to children, such that the first child shows the error and the others are greyed out.")
	;

	Rcpp::class_<jaspContainer_Interface>("jaspContainer")
		.derives<jaspObject_Interface>("jaspObject")

		.property("length",							&jaspContainer_Interface::length,																		"Returns how many objects are stored in this container.")
		.property("initCollapsed",					&jaspContainer_Interface::getInitiallyCollapsed,	&jaspContainer_Interface::setInitiallyCollapsed,	"If this is set true the container will be collapsed initially.")
		.method( "[[",								&jaspContainer_Interface::at,																			"Retrieve an object from this container as specified under the fieldname.")
		.method( "[[<-",							&jaspContainer_Interface::insert,																		"Insert an object into this container under a fieldname, if this object is a jaspObject and without a title it will get the fieldname as title.")
		.method( "findObjectWithUniqueNestedName",	&jaspContainer_Interface::findObjectWithUniqueNestedName,												"Find a jasp object from its unique name")
	;

	Rcpp::class_<jaspPlot_Interface>("jaspPlot")
		.derives<jaspObject_Interface>("jaspObject")

		.property("aspectRatio",	&jaspPlot_Interface::getAspectRatio,	&jaspPlot_Interface::setAspectRatio,	"Stores the aspect ratio used to make the plot, will not redraw the plot on change.")
		.property("width",			&jaspPlot_Interface::getWidth,			&jaspPlot_Interface::setWidth,			"Stores the width used to make the plot, will not redraw the plot on change.")
		.property("height",			&jaspPlot_Interface::getHeight,			&jaspPlot_Interface::setHeight,			"Stores the height used to make the plot, will not redraw the plot on change.")
		.property("status",         &jaspPlot_Interface::getStatus,			&jaspPlot_Interface::setStatus,         "Stores the status of the plot, default is complete, set to 'running' if it takes a long time to calculate it.")
		.property("filePathPng",	&jaspPlot_Interface::getFilePathPng,	&jaspPlot_Interface::setFilePathPng,	"Stores the filepath to the image-file generated by the plot.")

		.property("plotObject",		&jaspPlot_Interface::getPlotObject,		&jaspPlot_Interface::setPlotObject,		"Stores the plotObj used to generate the graphic, will (should) be stored in a way that is later accesible to saveImage an editImage.")

		.property("editing",		&jaspPlot_Interface::getEditing,		&jaspPlot_Interface::setEditing,		"If set to true will overwrite current png file when rendering a plot.")
		.property("revision",		&jaspPlot_Interface::getRevision,												"return the current revision of the plot.")
	;

	JASPLIST_MODULE_EXPORT(jaspStringlist_Interface,	"jaspStringlist")
	JASPLIST_MODULE_EXPORT(jaspDoublelist_Interface,	"jaspDoublelist")
	JASPLIST_MODULE_EXPORT(jaspIntlist_Interface,		"jaspIntlist")
	JASPLIST_MODULE_EXPORT(jaspBoollist_Interface,		"jaspBoollist")

	const std::string addRowsGeneralDoc =
			"Before the data is added all existing columns will be made the same length by appending null-values. "
			"If the new data contains more columns than currently present empty columns will be added. "
			"Columnnames will be extracted and used to place the data in the correct column, they can be specified through the elementnames of a list, names of a data.frame and colnames of a matrix. "
			"To also set the rownames you can fill pass a characterVector with the desired names in the second argument.";

	const std::string addRowsDoc = "Add rows to the table, where 'rows' is a list (of rows), dataframe or matrix. " + addRowsGeneralDoc;
	const std::string addRowDoc  = "Add a row to the table, where 'rows' is a list (of values) or vector. " + addRowsGeneralDoc;


	Rcpp::class_<jaspTable_Interface>("jaspTable")
		.derives<jaspObject_Interface>("jaspObject")

		.property("colNames",					&jaspTable_Interface::getColNames,					"List of columnnames, single elements can be get and set here directly through [['']] notation but setting all columnnames at once should be done through setColNames")
		.method("setColNames",					&jaspTable_Interface::setColNames,					"Accepts a list of strings to be used as columnnames, if the elements are named they will be accessible later through fieldname.")

		.property("colTypes",					&jaspTable_Interface::getColTypes,					"List of columntypes, single elements can be get and set here directly through [['']] notation but setting all columntypes at once should be done through setColTypes")
		.method("setColTypes",					&jaspTable_Interface::setColTypes,					"Accepts a list of strings to be used as columntypes, if the elements are named they will be accessible later through fieldname.")

		.property("colTitles",					&jaspTable_Interface::getColTitles,					"List of columntitles, single elements can be get and set here directly through [['']] notation but setting all columntitles at once should be done through setColTitles.\nIf fieldnames are used to set the title (aka aTable$colTitles[['some text']]) then this will override any columntitle set by index for that specific columnname. What this means is that if the first column is named 'a' and you set both \"colTitles[[0]] <- 'one'\" and \"colTitles[['a']] <- 'two'\" than the first column will have 'two' as its title if it's name is 'a'.")
		.method("setColTitles",					&jaspTable_Interface::setColTitles,					"Accepts a list of strings to be used as columntitles, if the elements are named they will be accessible later through fieldname.")

		.property("colOvertitles",				&jaspTable_Interface::getColOvertitles,				"List of columnovertitles, single elements can be get and set here directly through [['']] notation but setting all columntitles at once should be done through setColTitles.\nIf fieldnames are used to set the title (aka aTable$colOvertitles[['some text']]) then this will override any columntitle set by index for that specific columnname. What this means is that if the first column is named 'a' and you set both \"colTitles[[0]] <- 'one'\" and \"colTitles[['a']] <- 'two'\" than the first column will have 'two' as its title if it's name is 'a'.")
		.method("setColOvertitles",				&jaspTable_Interface::setColOvertitles,				"Accepts a list of strings to be used as columnovertitles, if the elements are named they will be accessible later through fieldname.")

		.property("colFormats",					&jaspTable_Interface::getColFormats,				"List of columnformats, single elements can be get and set here directly through [['']] notation but setting all columnformats at once should be done through setColFormats")
		.method("setColFormats",				&jaspTable_Interface::setColFormats,				"Accepts a list of strings to be used as columnformats, if the elements are named they will be accessible later through fieldname.")

		.property("colCombines",				&jaspTable_Interface::getColCombines,				"List of column combines, single elements can be get and set here directly through [['']] notation but setting all columncombines at once should be done through setColFormats")
		.method("setColCombiness",				&jaspTable_Interface::setColCombines,				"Accepts a list of logicals to be used as columncombines, if the elements are named they will be accessible later through fieldname.")

		.property("rowNames",					&jaspTable_Interface::getRowNames,					"List of rownames, single elements can be get and set here directly through [['']] notation but setting all rownames at once should be done through setRowNames")
		.method("setRowNames",					&jaspTable_Interface::setRowNames,					"Accepts a list of strings to be used as rownames, if the elements are named they will be accessible later through fieldname.")

		.property("rowTitles",					&jaspTable_Interface::getRowTitles,					"List of rowtitles, single elements can be get and set here directly through [['']] notation but setting all rowtitles at once should be done through setRowTitles. This will respond in a similar manner to conflicts between an indexed title and a fieldnamed title. See documentation of colTitles.")
		.method("setRowTitles",					&jaspTable_Interface::setRowTitles,					"Accepts a list of strings to be used as rowtitles, if the elements are named they will be accessible later through fieldname.")

		.method("addColumnInfoHelper",			&jaspTable_Interface::addColumnInfo,				"addColumnInfoHelper(name=NULL, title=NULL, type=NULL, format=NULL, combine=NULL) -> Adds column info, an entry to columName wether you specify it or not and if the others are not NULL then they are set for the column")

		.method("addFootnoteHelper",			&jaspTable_Interface::addFootnote,					"addFootnoteHelper(message="", symbol=NULL, column=NULL, row=NULL) === Add a footnote to the table, if column or row is not -1 it will be added to the specified column or row, if both are changed then it will be a footnote on a cell and otherwise it will just be a footnote of the entire table. A symbol may also be specified.")

		.method("setData",						&jaspTable_Interface::setData,						"Set the data of the table, this accepts lists, dataframes, matrices and vectors. If any column- or rownames are specified they are set on the object, but only if they aren't set yet. Any one-dimensional data given will be assumed to be a row.")


		.method("addRows",						&jaspTable_Interface::addRows,						addRowsDoc.c_str())
		.method("addRows",						&jaspTable_Interface::addRowsWithoutNames,			addRowsDoc.c_str())

		.method("addRow",						&jaspTable_Interface::addRow,						addRowDoc.c_str())
		.method("addRow",						&jaspTable_Interface::addRowWithoutNames,			addRowDoc.c_str())

		//.method("addColumn",					&jaspTable_Interface::addColumns,					"Add one or more columns to the object, this class accepts the same datatypes as setdata.  Column- and rownames will be extracted as well but used only if the corresponding names aren't set yet.")
		.method("addColumns",					&jaspTable_Interface::addColumns,					"Add one or more columns to the object, this class accepts the same datatypes as setdata.  Column- and rownames will be extracted as well but used only if the corresponding names aren't set yet.")
		// is [[<- === .method("setColumn",					&jaspTable_Interface::setColumn,					"setColumn(columnName, columnData): set a vector or a list as a column for columnName.")

		.property("showSpecifiedColumnsOnly",	&jaspTable_Interface::getShowSpecifiedColumnsOnly,
												&jaspTable_Interface::setShowSpecifiedColumnsOnly,	"If set to true will make only the specified columns (through addColumnInfo etc) show in the results.")

		.property("transposeWithOvertitle",		&jaspTable_Interface::getTransposeWithOvertitle,
												&jaspTable_Interface::setTransposeWithOvertitle,	"If set to true in combination with transpose == true it will use the first column of the data as overtitle.")

		.property("transpose",					&jaspTable_Interface::getTransposeTable,
												&jaspTable_Interface::setTransposeTable,			"If set to true will swap rows and columns in the results.")

		.property("status",						&jaspTable_Interface::getStatus,
												&jaspTable_Interface::setStatus,					"The status of the table, usually (and by default) 'complete'")



		.method( "[[<-",						&jaspTable_Interface::setColumn,					"Insert a single column into the table, if a string is used then it will look for an existing column name and set that column with the new data and otherwise will just add it at the end. If it is indexed by integer it will simply set it there.")

		.method("setExpectedSize",				&jaspTable_Interface::setExpectedSize,				"Set the expected size of this table to the specified columnCount and rowCount. It will make your table show up, filled with dots, at this size and as you add data the dots will be replaced with it.")
		.method("setExpectedColumns",			&jaspTable_Interface::setExpectedColumns,				"Set the expected size of this table to the specified columnCount. It will make your table show up, filled with dots, at this size and as you add data the dots will be replaced with it.")
		.method("setExpectedRows",				&jaspTable_Interface::setExpectedRows,				"Set the expected size of this table to the specified rowCount. It will make your table show up, filled with dots, at this size and as you add data the dots will be replaced with it.")
	;

	Rcpp::class_<jaspHtml_Interface>("jaspHtml")
		.derives<jaspObject_Interface>("jaspObject")
		.property("text",			&jaspHtml_Interface::getText,			&jaspHtml_Interface::setText,			"The text of this element")
        .property("html",			&jaspHtml_Interface::getHtml,													"The text of this element")
		.property("elementType",	&jaspHtml_Interface::getElementType,	&jaspHtml_Interface::setElementType,	"The type of this html element, default is 'p' but other useful values include 'H1', 'h2' etc. If you want to write your own html element completely set this to \"\"")
		.property("class",			&jaspHtml_Interface::getClass,			&jaspHtml_Interface::setClass,			"The Css-class of this element, for monospace one could use jasp-code or simply leave it empty.")
		.property("maxWidth",		&jaspHtml_Interface::getMaxWidth,		&jaspHtml_Interface::setMaxWidth,		"The Css-max-width property. It will be set on a span around your html.")
	;


	Rcpp::class_<jaspState_Interface>("jaspState")
		.derives<jaspObject_Interface>("jaspObject")
		.property("object", &jaspState_Interface::getObject, &jaspState_Interface::setObject, "The object that you might want to keep for the next revision of your analysis.")
	;

	Rcpp::class_<jaspColumn_Interface>("jaspColumn")
		.derives<jaspObject_Interface>("jaspObject")
		.method("setScale",				&jaspColumn_Interface::setScale,		"Overwrite the contents of the specified column with scalar data.")
		.method("setOrdinal",			&jaspColumn_Interface::setOrdinal,		"Overwrite the contents of the specified column with ordinal data.")
		.method("setNominal",			&jaspColumn_Interface::setNominal,		"Overwrite the contents of the specified column with nominal data.")
		.method("setNominalText",		&jaspColumn_Interface::setNominalText,	"Overwrite the contents of the specified column with nominal text data.")
	;

	Rcpp::class_<jaspResults_Interface>("jaspResultsClass")
		.derives<jaspContainer_Interface>("jaspContainer")
		.method("send",						&jaspResults_Interface::send,									"Constructs the results/response-json and sends it to Desktop, but only if jaspResults::setSendFunc was called with an appropriate sendFuncDef first.")
		.method("complete",					&jaspResults_Interface::complete,								"Constructs the results/response-json and sends it to Desktop but sets status to complete first.")
		.method("setErrorMessage",			&jaspResults_Interface::setErrorMessage,						"Sets an errormessage on the results.")
		.method("getPlotObjectsForState",	&jaspResults_Interface::getPlotObjectsForState,					"Retrieves all plot objects and stores them in a list with the filePath of the plot as name of the element.")
		.method("getOtherObjectsForState",	&jaspResults_Interface::getOtherObjectsForState,				"Retrieves all non-plot objects to store them in state (currently only jaspState objects) . Makes a list with the envName of the object as name of the element.")


		.method("getKeepList",				&jaspResults_Interface::getKeepList,							"Builds a list of filenames to keep.")

		.property("status",					&jaspResults_Interface::getStatus,
											&jaspResults_Interface::setStatus,								"The status of the jaspResults object")

		.property("relativePathKeep",		&jaspResults_Interface::getRelativePathKeep,
											&jaspResults_Interface::setRelativePathKeep,					"The relative path to where state is kept")

		.method("getResults",				&jaspResults_Interface::getResults,								"Returns the latest version of the results json as a string")

		.method("setOptions",				&jaspResults_Interface::setOptions,								"Tells jaspResults which options are currently set, should not be used in an analysis!")
		.method("changeOptions",			&jaspResults_Interface::changeOptions,							"Changes the currently set options and removes all objects that depend on the changed options. Mostly useful for unit tests because this we we can simulate re-running the analysis. Should not be used in an analysis!")

		.method("prepareForWriting",		&jaspResults_Interface::prepareForWriting,						"Remove seal for writing")
		.method("finishWriting",			&jaspResults_Interface::finishWriting,							"Set seal for writing")
		.method("saveResults",				&jaspResults_Interface::saveResults,							"save results")
	;

	Rcpp::class_<jaspQmlSource_Interface>("jaspQmlSource")
		.derives<jaspTable_Interface>("jaspTable")
		.property("sourceID",				&jaspQmlSource_Interface::getSourceID,
											&jaspQmlSource_Interface::setSourceID,							"The name of the qml object for which this r-source is meant.");
}


Rcpp::RObject givejaspResultsModule()
{
	/* Should be done through LOAD_RCPP_MODULE actually but there seems to be something wrong with the macro.. So we just copy the underlying code like  https://stackoverflow.com/questions/45344260/load-rcpp-module-seems-not-right */
	Rcpp::Shield<SEXP> __load_module_call__( Rf_lang2( GET_MODULE_SYM, _rcpp_module_boot_jaspResults() ) );
	return Rcpp_eval( __load_module_call__, R_GlobalEnv );
}
