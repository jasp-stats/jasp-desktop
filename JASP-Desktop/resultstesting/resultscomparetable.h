#ifndef RESULTSCOMPARETABLE_H
#define RESULTSCOMPARETABLE_H

#include <string>
#include <vector>

namespace resultXmlCompare
{

class tableCell
{
public:
	bool		isEqual(const tableCell & other) const;

	bool		isHeader()					{ return isHeaderCell;		}
	void		setHeader(bool header)		{ isHeaderCell = header;	}

	const std::string &	value()						{ return _value;	}
	void				setValue(std::string val)	{ _value = val;		}
	void				addToValue(std::string val) { _value += val;	}

	std::string			toString() const;
	std::string			diffToString(const tableCell & other) const;

private:
	bool		isHeaderCell;
	std::string	_value;
};

typedef std::vector<tableCell> tableCells;

inline bool operator==(const tableCell& lhs,	const tableCell& rhs)	{ return lhs.isEqual(rhs); }
inline bool operator!=(const tableCell& lhs,	const tableCell& rhs)	{ return !(lhs == rhs); }

class tableRow
{
public:
	bool		isEqual(const tableRow & other) const;
	void		genHeaderCell();
	void		genBodyCell();
	tableCell &	curCell();

	std::string	toString() const;
	std::string	diffToString(const tableRow & other) const;

private:
	void		genCell();
	tableCells	cells;
};

typedef std::vector<tableRow> tableRows;

inline bool operator==(const tableRow& lhs,		const tableRow& rhs)	{ return lhs.isEqual(rhs); }
inline bool operator!=(const tableRow& lhs,		const tableRow& rhs)	{ return !(lhs == rhs); }


class tableBlock
{
public:
	bool		isEqual(const tableBlock & other) const;

	void		genRow();
	tableRow &	curRow();

	std::string	toString() const;
	std::string	diffToString(const tableBlock & other) const;

private:
	tableRows	rows;
};

inline bool operator==(const tableBlock& lhs,	const tableBlock& rhs)	{ return lhs.isEqual(rhs); }
inline bool operator!=(const tableBlock& lhs,	const tableBlock& rhs)	{ return !(lhs == rhs); }

class table
{
public:
	bool			isEqual(const table & other) const;

	tableBlock *	head() { return &_head; }
	tableBlock *	body() { return &_body; }
	tableBlock *	foot() { return &_foot; }

	std::string		toString() const;
	std::string		diffToString(const table & other) const;

private:
	tableBlock	_head,
				_body,
				_foot;
};

typedef std::vector<table> tables;

inline bool operator==(const table& lhs,		const table& rhs)		{ return lhs.isEqual(rhs); }
inline bool operator!=(const table& lhs,		const table& rhs)		{ return !(lhs == rhs); }

struct result
{
public:
	bool		isEqual(const result & other) const;

	void		genTable();
	table &		curTable();

	std::string	toString() const;
	std::string	diffToString(const result & other) const;

	bool		hasError() { return _error; }
	void		setError() { _error = true; }

private:
	tables		resultTables;
	bool		_error = false;
};


inline bool operator==(const result& lhs,		const result& rhs)		{ return lhs.isEqual(rhs); }
inline bool operator!=(const result& lhs,		const result& rhs)		{ return !(lhs == rhs); }

}

#endif // RESULTSCOMPARETABLE_H
