//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef TERM_H
#define TERM_H

#include <vector>
#include <string>

#include <QString>
#include <QStringList>
#include "columntype.h"
#include <json/json.h>

///
/// A term is a basic element of a VariablesList
/// It is usually just a string, but in case of interactions, it is a vector of strings, a component being one part of an interaction.
///
/// A term also carries its provenance (mds-plan C9): the id of the dataset/filter selection it was
/// selected THROUGH (`filterId()`, -1 = unqualified). Like `types` this is internal typed state —
/// nothing of it reaches the JSON wire in this phase (the generators of mds-plan Phase 3 own the
/// serialization) — but it IS part of identity: two same-name terms from different filters never
/// compare equal, which is what lets one list hold e.g. "TestInts" of two different slices at the
/// same time. The default copy constructor/assignment carry it (plain member, no custom copies).
///
class Term
{
public:
	Term(const std::vector<std::string> &	components, const columnTypeVec	&	types	= { columnType::unknown }	);
	Term(const std::string				&	component,	const columnType		type	= columnType::unknown		);
	Term(const std::string				&	component,	const columnTypeVec	&	type);
	Term(const QStringList				&	components, const columnTypeVec	&	types	= { columnType::unknown }	);
	Term(const QString					&	component,	const columnType		type	= columnType::unknown		);
	Term(const QString					&	value,		const QString		&	label,		const QString		& info = QString(),		const columnType type = columnType::unknown);
	Term(const Json::Value				&	json,		const std::string	&	keyValue,	const std::string	& keyLabel, const columnTypeVec& types);

	const QStringList		&	components()			const;
	const QString			&	label()					const { return _label;	}
	const QString			&	value()					const { return _value;	}
	const QString			&	info()					const { return _info;	}
	std::vector<std::string>	scomponents()			const;

	bool						isDraggable()	const			{ return _draggable; }
	void						setDraggable(bool draggable)	{ _draggable = draggable; }

	// If a term has several components, its type self is unknown, but the components have maybe a type.
	columnType					type()			const			{ return _types.size() == 1 ? _types[0] : columnType::unknown; }
	void						setType(columnType type)		{ _types = {type};	}
	columnTypeVec				types()			const			{ return _types;	}
	void						setTypes(columnTypeVec types)	{ _types = types;	}
	void						setLabel(const QString& label)	{ _label = label;	}

	///< mds-plan C9: id of the filter this term was selected THROUGH, -1 while unqualified. Part of
	///  the term's identity (see operator==); stamped by the list the term lives in (ListModel).
	int							filterId()		const			{ return _filterId;	}
	void						setFilterId(int filterId)			{ _filterId = filterId;	}

	typedef QStringList::const_iterator const_iterator;
	typedef QStringList::iterator		iterator;

	bool contains(		const QString	& component)	const;
	bool containsAll(	const Term		& term)			const;
	bool containsAny(	const Term		& term)			const;

	iterator begin();
	iterator end();

	const QString &at(int i) const;

	bool operator==(const Term &other) const;
	bool operator!=(const Term &other) const;
	///< Total order over the term's identity: by value, filterId as tiebreaker so same-name terms
	///  from different filters stay distinguishable in ordered containers (std::map<Term, ...>).
	bool operator<(const Term &other) const;

	///< Hash consistent with operator== (mds-plan C9): == compares the components as a SET and the
	///  filterId, so this mixes per-component hashes (xor: order-free), the size and the filterId.
	friend size_t qHash(const Term &term, size_t seed);

	size_t size() const;

	bool replaceVariableName(const std::string & oldValue, const std::string & newValue);

	static const char* separator;
	static Term	readTerm(std::string str);
	static Term	readTerm(QString str);

	Json::Value toJson(bool useArray = true, bool useValueAndType = true) const;

private:
	void initFrom(const QStringList	& components,	const QString	& label, const columnTypeVec& type, const QString	& info = "");
	void initFrom(const QString		& value,		const QString	& label, const columnTypeVec& type,	const QString	& info = "");

	QStringList		_components;
	QString			_label,
					_value,
					_info;
	bool			_draggable = true;
	columnTypeVec	_types = {columnType::unknown};
	int				_filterId = -1;			///< mds-plan C9: filter selected through, -1 = unqualified; part of identity
};

inline size_t qHash(const Term &term, size_t seed)
{
	size_t hash = ::qHash(static_cast<uint>(term._components.size()), seed);
	for(const QString &component : term._components)
		hash ^= ::qHash(component, seed);
	return hash ^ ::qHash(term._filterId, seed);
}

#endif // TERM_H
