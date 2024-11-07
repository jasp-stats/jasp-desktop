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
class Term
{
public:
	Term(const std::vector<std::string> components, const columnTypeVec& types	= { columnType::unknown }	);
	Term(const std::string				component, columnType type				= columnType::unknown		);
	Term(const QStringList				components, const columnTypeVec& types	= { columnType::unknown }	);
	Term(const QString					component, columnType type				= columnType::unknown		);

	const QStringList			& components()	const;
	const QString				& asQString()	const;

	std::vector<std::string>	scomponents()	const;
	std::string					asString()		const;

	bool						isDraggable()	const			{ return _draggable; }
	void						setDraggable(bool draggable)	{ _draggable = draggable; }

	// If a term has several components, its type self is unknown, but the components have maybe a type.
	columnType					type()			const			{ return _types.size() == 1 ? _types[0] : columnType::unknown; }
	void						setType(columnType type)		{ _types = {type}; }
	columnTypeVec				types()			const			{ return _types; }
	void						setTypes(columnTypeVec types)	{ _types = types; }

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
	bool operator<(const Term &other) const;

	size_t size() const;

	bool replaceVariableName(const std::string & oldName, const std::string & newName);

	static const char* separator;
	static Term	readTerm(std::string str);
	static Term	readTerm(QString str);
	static Term readTerm(const Json::Value& json, columnType defaultType = columnType::unknown);

	Json::Value toJson(bool useArray = true, bool useValueAndType = true) const;

private:
	void initFrom(const QStringList components, const columnTypeVec& type);
	void initFrom(const QString		component,	columnType type);

	QStringList		_components;
	QString			_asQString;
	bool			_draggable = true;
	columnTypeVec	_types = {columnType::unknown};
};

#endif // TERM_H
