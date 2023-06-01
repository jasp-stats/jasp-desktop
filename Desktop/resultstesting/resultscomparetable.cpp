#include "resultscomparetable.h"
#include <sstream>
#include <algorithm>

namespace resultXmlCompare
{

bool tableCell::isEqual(const tableCell & other) const
{
	return isHeaderCell == other.isHeaderCell && _value == other._value;
}

bool tableRow::isEqual(const tableRow & other) const
{
	if(cells.size() != other.cells.size())
		return false;

	for(size_t cell=0; cell < cells.size(); cell++)
		if(cells[cell] != other.cells[cell])
			return false;

	return true;
}


void tableRow::genHeaderCell()
{
	genCell();
	curCell().setHeader(true);
}

void tableRow::genBodyCell()
{
	genCell();
	curCell().setHeader(false);
}

void tableRow::genCell()
{
	cells.resize(cells.size() + 1);
}

tableCell & tableRow::curCell()
{
	if(cells.size() == 0)
		genBodyCell();

	return cells[cells.size() - 1];
}

bool tableBlock::isEqual(const tableBlock & other) const
{
	if(rows.size() != other.rows.size())
		return false;

	for(size_t row=0; row < rows.size(); row++)
		if(rows[row] != other.rows[row])
			return false;

	return true;
}

void tableBlock::genRow()
{
	rows.resize(rows.size() + 1);
}

tableRow & tableBlock::curRow()
{
	if(rows.size() == 0)
		genRow();

	return rows[rows.size() - 1];
}


bool table::isEqual(const table & other) const
{
	return _head == other._head && _body == other._body && _foot == other._foot;
}


bool result::isEqual(const result & other) const
{
	if(resultTables.size() != other.resultTables.size())
		return false;

	for(size_t resultTable=0; resultTable < resultTables.size(); resultTable++)
		if(resultTables[resultTable] != other.resultTables[resultTable])
			return false;

	return true;
}

void result::genTable()
{
	resultTables.resize(resultTables.size() + 1);
}

table & result::curTable()
{
	if(resultTables.size() == 0)
		genTable();

	return resultTables[resultTables.size() - 1];
}

std::string tableCell::toString() const
{
	return (isHeaderCell ? "h: " : "") + _value;
}

std::string tableCell::diffToString(const tableCell & other) const
{
	if(isEqual(other))
		return toString();

	return toString() + " != " + other.toString();
}

std::string	tableRow::toString() const
{
	std::stringstream out;

	out << "\t\t\tRow: | ";
	for(auto & cell : cells)
		out << cell.toString() << " | ";
	out << "\n";

	return out.str();
}

std::string	tableRow::diffToString(const tableRow & other) const
{
	if(isEqual(other))
		return "\t\t\tRows are the same\n";

	std::stringstream out;

	out << "\t\t\tRow: | ";

	size_t maxCell = std::max(cells.size(), other.cells.size());

	for(size_t cellNum = 0; cellNum < maxCell; cellNum++)
		if(cellNum < cells.size() && cellNum < other.cells.size())
			out << cells[cellNum].diffToString(other.cells[cellNum]) << " | ";
		else
		{
			std::string cellOld = cellNum >= cells.size()		? "missing" : cells[cellNum].toString();
			std::string cellNew = cellNum >= other.cells.size() ? "missing" : other.cells[cellNum].toString();

			out << cellOld << " != " << cellNew << " | ";
		}

	out << "\n";
	return out.str();
}

std::string	tableBlock::toString() const
{
	std::stringstream out;

	out << "\t\tRows:\n";

	for(auto & row : rows)
		out << row.toString();

	return out.str();
}

std::string	tableBlock::diffToString(const tableBlock & other) const
{
	if(isEqual(other))
		return "\t\tTable sections are the same\n";

	std::stringstream out;

	out << "\t\tRows:\n";

	size_t maxRow = std::max(rows.size(), other.rows.size());

	for(size_t rowNum = 0; rowNum < maxRow; rowNum++)
		if(rowNum < rows.size() && rowNum < other.rows.size())	out << rows[rowNum].diffToString(other.rows[rowNum]);
		else if(rowNum >= rows.size())							out << "\t\t\tRow is missing in old table section\n";
		else													out << "\t\t\tRow is missing in new table section\n";

	return out.str();
}


std::string	table::toString() const
{
	std::stringstream out;

	out << "\tHeader:\n"	<< _head.toString() << "\n";
	out << "\tBody:\n"		<< _body.toString() << "\n";
	out << "\tFooter:\n "	<< _foot.toString() << "\n";

	return out.str();
}

std::string	table::diffToString(const table & other) const
{
	if(isEqual(other))
		return "\tTables are the same\n";

	std::stringstream out;

	out << "\tHeader:\n"	<< _head.diffToString(other._head) << "\n";
	out << "\tBody:\n"		<< _body.diffToString(other._body) << "\n";
	out << "\tFooter:\n "	<< _foot.diffToString(other._foot) << "\n";

	return out.str();
}


std::string	result::toString() const
{
	std::stringstream out;
	out << "Tables:\n";

	for(auto & table : resultTables)
		out << table.toString() << "\n";

	return out.str();
}

std::string	result::diffToString(const result & other) const
{
	if(isEqual(other))
		return "Results are the same\n";

	std::stringstream out;

	if(resultTables.size() != other.resultTables.size())
	{
		out << "Results have different count of tables\n";
		out << "Old has " << resultTables.size() << " while new has " << other.resultTables.size() << "\nTables compared:\n";

		size_t maxCount = std::max(resultTables.size(), other.resultTables.size());

		for(size_t i=0; i<maxCount; i++)
			if(i < resultTables.size() && i < other.resultTables.size())
				out << "Table " << i << ":\n" << resultTables[i].diffToString(other.resultTables[i]) << "\n";
			else if(i < other.resultTables.size())
				out << "Table " << i << " is only in new:\n" << other.resultTables[i].toString() << "\n";
			else
				out << "Table " << i << " is only in old:\n" << resultTables[i].toString() << "\n";
	}
	else
	{
		out << "Tables:\n";

		for(size_t i=0; i<resultTables.size(); i++)
			out << resultTables[i].diffToString(other.resultTables[i]) << "\n";
	}

	return out.str();

}


}
