//
// Copyright (C) 2013-2017 University of Amsterdam
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

class Term
{
public:
	Term(const std::vector<std::string> components);
	Term(const std::string component);
	Term(const QStringList components);
	Term(const QString component);

	const QStringList &components() const;
	const std::vector<std::string> &scomponents() const;
	const QString &asQString() const;
	const std::string &asString() const;

	typedef QStringList::const_iterator const_iterator;
	typedef QStringList::iterator iterator;

	bool contains(const std::string &component) const;
	bool containsAll(const Term &term) const;
	bool containsAny(const Term &term) const;

	iterator begin();
	iterator end();

	const QString &at(int i) const;

	bool operator==(const Term &other) const;
	bool operator!=(const Term &other) const;

	size_t size() const;

private:
	QStringList _components;
	std::vector<std::string> _scomponents;
	std::string _asString;
	QString _asQString;

};

#endif // TERM_H
