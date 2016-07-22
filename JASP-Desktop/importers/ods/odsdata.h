#ifndef ODSDATA_H
#define ODSDATA_H


#include "QString"
#include <column.h>

#include <set>
#include <vector>
#include <map>

#include <dataset.h>

namespace ods
{


/**
 * @brief The ODSData class Holds data from an ODS file.
 */
class Data
{
public:

	// cell data types in sheet.
	typedef enum e_odsDataType
	{
		odsType_unknown	= 0,
		odsType_float,
		odsType_currency,
		odsType_percent,
		odsType_boolean,
		odsType_string = -1,
		odsType_date = -2,
		odsType_time = -3
	} XmlDatatype;

	// Holds a numeric value.
	typedef union s_numbers
	{
		double	dbl;
		int		i;
	} NumericValue;

	class SheetColumn;

	// Holds data from the XML cell.
	class SheetCellShort
	{
	public:
		/**
		 * @brief SheetCellShort Create a short data cell.
		 * @param row row number (0 based)
		 * @param type Data type from XML.
		 * @param data Value (as string)
		 */
		SheetCellShort(const SheetColumn *col, int row, XmlDatatype type, const QString& xmlData);

		~SheetCellShort() {}

		// Getters and setters.
		void type(XmlDatatype type) { _type = type; }
		const XmlDatatype& type() const { return _type; }
		const QString& data() const{ return _data; }
		double numData() const { return _numData; }
		int rowNumber() const{ return _rowNumber; }
		bool isNotEmpty() const { return _notEmpty; }

	private:
		XmlDatatype			_type;
		QString				_data;
		double				_numData;
		int					_rowNumber;
		const SheetColumn  *_column;
		bool				_notEmpty;
	};

	// As SheetCellShort, but either data OR fltdata is set.
	class SheetCellLong
	{
	public:

		const static int EmptyInt;
		const static double EmptyDouble;
		const static std::string EmptyString;

		SheetCellLong();
		SheetCellLong(const SheetCellShort &cellShort);

		~SheetCellLong() {}

		bool operator < (const SheetCellLong &rhs) const;

		// Getters and setters.
		void xmlType(XmlDatatype type) { _xmlType = type; }
		const XmlDatatype& xmlType() const { return _xmlType; }

		void jaspType(Column::ColumnType type) { _jaspType = type; }
		const Column::ColumnType& jaspType() const { return _jaspType; }
		bool isNotEmpty() const { return _notEmpty; }

		int valueAsInt() const;
		double valueAsDouble() const;
		std::string valueAsString() const;

		/**
		 *
		 * @brief forceCellToType Force Cell contents to type.
		 * @param requiredType Type to force.
		 * @param column int Colum number (for error reporting)
		 *
		 * Throws an exception on fail.
		 */
		void forceCellToType(Column::ColumnType requiredType);


	private:
		XmlDatatype			_xmlType;
		Column::ColumnType	_jaspType;
		QString				_data;
		NumericValue		_numData;
		bool				_notEmpty;
		const SheetColumn*  _column;


		void _convertToScalar();
		void _convertToNominal();
		void _convertToNominalText();

	};

	class JaspColumn
	{

	public:
		JaspColumn()
			: _colType(Column::ColumnTypeUnknown)
		{}

		Column::ColumnType type() const { return _colType; }
		void type(Column::ColumnType type) { _colType = type; }
		const std::string& lable() const { return _label; }
		void label(const std::string& label) { _label = label; }

	private:
		Column::ColumnType _colType;
		std::string	_label;
	};

	/**
	 * @brief The Column class Holds one column of data.
	 */
	class SheetColumn
	{
	public:

		static const SheetCellLong EmptyLongCell;
		static const SheetCellShort EmptyShortCell;

		SheetColumn(int colNumber) : _columnNumber(colNumber) {}

		/**
		 * @brief insert Inserts one cell.
		 * @param row Row to insert
		 * @param type ODS data type.
		 * @param data ODS cell value.
		 */
		void insert(int row, XmlDatatype type, const QString& data);


		/**
		 * @brief getScell GEts an unprocessed cell.
		 * @param row row to fetch
		 * @return Empty cell if cell donsn't exist.
		 */
		const SheetCellShort &getScell(int row) const;

		/**
		 * @brief process Process the read data.
		 * @return Found Jasp Type and label.
		 *
		 * No Getters work until this method has been called.
		 */
		JaspColumn process();

		/**
		 * @brief strings finds all unique strings.
		 * @return A vector of strings.
		 */
		std::vector<QString> strings() const;

		/**
		 * @brief hasCall Checks for presence of a cell at row.
		 * @param row Row check for.
		 * @return true if value in column.
		 */
		inline bool hasCell(int row) const
		{
			return (_index.find(row) != _index.end());
		}

		/**
		 * @brief getLCell Gets a long cell by row.
		 * @param row
		 * @return Reference to long cell
		 */
		const SheetCellLong& getLCell(int row) const;
		SheetCellLong *getLCell(int row);

		/**
		 * @brief value Finds the value for the row.
		 * @param row The row to search for (0 - for column lable)
		 * @return Value or Appropriate if none.
		 */
		int		valueAsInt(int row) const;
		double	valueAsDouble(int row) const;

		/**
		 * @brief numberLabels The number of labels we can have.
		 * @return
		 *
		 * Return value is the maximum plus one tha can be passed to lableAt(int).
		 */
		int numberLabels() const { return _processedCells.size(); }

		/**
		 * @brief labelAt Fetches the string value for index value (NOT the row!)
		 * @param index Cell index to fetch for/
		 * @return Emptry string on invlide index.
		 */
		std::string labelAt(int index) const;

		/**
		 * @brief type Finds the type for the row.
		 * @param row The row to search for (0 - for column lable)
		 * @return Type for the cell.
		 */
		Column::ColumnType type(int row) const;

		/**
		 * @brief numRows Finds the min or maximum row number.
		 * @return The number of index we have.
		 */
		int minRow() const;
		int maxRow() const;

		/**
		 * @brief colNumberAsExcel Returns the column number as a string (base 26 A-Z).
		 * @return
		 */
		std::string colNumberAsExcel() const;

	private:
		typedef std::vector<SheetCellShort>	ShortCellSet;
		ShortCellSet _cells;	///< The data (from the XML).

		typedef std::map< int, size_t > CellIndex;
		CellIndex	_index;		///< cell indexes indexed by row.
		typedef std::vector<SheetCellLong> ProcessedCells;
		ProcessedCells _processedCells; //< cell after processing
		int		_columnNumber;

		/**
		 * @brief _forceToType Force all cells to the type.
		 * @param type Type to force.
		 */
		void _forceToType(Column::ColumnType type);
	};

	// Representation of Data.
	class Sheet : public std::vector< SheetColumn >
	{
	public:
		Sheet() {}
		~Sheet() {}

		/**
		 * @brief createSpace Ensure that we have enough space for the cell at row.
		 * @param column
		 * @return The number of columns available. - Maybe greater than column.
		 */
		size_t createSpace(int column);

		// Getters.
		int numColumns() const { return size(); }
		int minRow() const;
		int maxRow() const;
		int numRows() const { return ((maxRow() - minRow()) + 1); }
	};


	/********************** End Defs. ************************/

	Data();

	/**
	 * @brief setContentFilename Sets the content file name.
	 * @param name file name
	 */
	void setContentFilename(QString &name)
		{ _contentFilename = name; }

	/**
	 * @brief getContentFilename Get the content file name.
	 * @return Contents file name. (Not path!)
	 */
	const QString &getContentFilename()
		{ return _contentFilename; }


	Sheet &sheet() { return _sheet;}

	/**
	 * @brief Post processes the data.
	 * @return The JaspColumn meta data.
	 */
	std::vector<JaspColumn> process();


	static const std::string manifestPath; //< manfiest file in archive.
	static const QString contentRegExpression; //< A Reg. Ex. for the content filename.

private:
	QString _contentFilename;
	Sheet _sheet;		///< Data captured.

};

} // end namespace

#endif // ODSDATA_H
