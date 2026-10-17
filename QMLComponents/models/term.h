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

	///< mds-plan D5.1: THE Storage term object `{name, filterId?, types}` - the clean typed-state
	///  file serialization (the Engine wire shape above stays frozen as today's R contract).
	///  `name` is the value string for a single-component term and the component array for an
	///  interaction; `types` is always the per-component type array; `filterId` is emitted only
	///  when the term is qualified (>= 0, C9) and omitted for -1 exactly like the Engine sidecar
	///  omits nulls. Re-enters through the Terms(json) applier, which recognizes the shape via
	///  isStorageTermObject() (see the collision rules documented there).
	Json::Value toJsonStorage() const;

	///< Strict shape test for the D5.1 Storage term object: an OBJECT with a `name` member
	///  (string or array of strings) AND at least one of `types` (string or array of strings) /
	///  `filterId` (int; an explicit null or an integral double counts as absent/that int - the
	///  QML->Json bridge turned every JS number into null for years), and NO other members.
	///  The member-set restriction is what keeps the
	///  applier honest: rows (keyValue + row-child controls), factor-level rows (`name` +
	///  `levels`) and the Engine `{value, types}` entry all fail this test and keep parsing
	///  byte-identically to today. The one tolerated collision: a row object under a keyValue of
	///  "name" carrying a child literally named `types` whose value is string/array-of-strings
	///  and no other children (no real form does this; the MDS plan accepts the ambiguity
	///  because the Engine shape never nests bare names next to `types` at row level).
	static bool isStorageTermObject(const Json::Value & json);

	///< Parses one Storage term object (isStorageTermObject-checked by the caller) into a typed
	///  Term incl. its `filterId`; `fallbackTypes` are used when the entry carries no `types`
	///  (mixed hand-written trees: positional types still apply).
	static Term fromStorageTermObject(const Json::Value & json, const columnTypeVec & fallbackTypes = {});

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

///
/// mds-plan D6.1: the identity of a ROW of a term list for the row-scoped satellite state
/// (Terms::RelatedValuesPerTerm and ListModel's row-controls map). Those maps used to be keyed
/// by term.value() — the NAME — so a list holding two rows with the same column behind different
/// filters (the C9 twins) shared, and silently clobbered, one entry: the second row's children
/// overwrote the first's, and both rows resolved to the same RowControls object.
/// TermRowKey is the minimal identity that separates twins — (value, filterId) — while staying
/// stable over type edits (types are not part of it, like Term::operator==) and over a host
/// restamp of the OTHER rows. Row lookups go through Terms::relatedValuesForTerm(): exact hit on
/// (value, filterId), else the (value, -1) entry (the unqualified key the JSON applier writes for
/// engine-shaped input, which the list stamps with its host on intake — that bind flow must keep
/// finding the loaded row children), else nothing.
///
class TermRowKey
{
public:
	TermRowKey() = default;
	explicit TermRowKey(const QString & name, int filterId = -1) : _name(name), _filterId(filterId) {}

	///< The key of a row's own term (mds-plan C9 identity: value + the filter it came through).
	static TermRowKey ofTerm(const Term & term)			{ return TermRowKey(term.value(), term.filterId()); }

	const QString	&	name()		const	{ return _name;		}
	int					filterId()	const	{ return _filterId;	}

	///< Lookup term for Terms::containsValue(Term)-style identity checks: exact when the key is
	///  qualified, name-only (the D2.1 wildcard) when filterId is -1. Type never takes part.
	Term asTerm() const { Term term(_name); term.setFilterId(_filterId); return term; }

	bool operator<(const TermRowKey & other) const
	{
		if (_name != other._name)	return _name < other._name;
		return _filterId < other._filterId;
	}
	bool operator==(const TermRowKey & other) const	{ return _name == other._name && _filterId == other._filterId; }
	bool operator!=(const TermRowKey & other) const	{ return !(*this == other); }

private:
	QString	_name;
	int		_filterId	= -1;
};

#endif // TERM_H
