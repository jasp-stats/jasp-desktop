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

#ifndef TERMS_H
#define TERMS_H

#include <vector>
#include <string>
#include <set>

#include <QString>
#include <QList>
#include <QByteArray>

#include "term.h"

class Terms
{
public:
	Terms(const QList<QList<QString> >						& terms,	Terms *parent = nullptr);
	Terms(const QList<QString>								& terms,	Terms *parent = nullptr);
	Terms(const std::vector<std::vector<std::string> >		& terms,	Terms *parent = nullptr);
	Terms(const std::vector<std::string>					& terms,	Terms *parent = nullptr);
	Terms(const QList<Term>									& terms,	Terms *parent = nullptr);
	Terms(																Terms *parent = nullptr);

	void set(const QList<QList<QString> >					& terms);
	void set(const QList<QString>							& terms);
	void set(const std::vector<Term>						& terms, bool isUnique = true);
	void set(const std::vector<std::string>					& terms);
	void set(const std::vector<std::vector<std::string> >	& terms);
	void set(const QList<Term>								& terms);
	void set(const Terms									& terms, bool isUnique = true);
	void set(const QByteArray								& array);

	void removeParent();
	void setSortParent(const Terms &parent);

	void add(const Term &term, bool isUnique = true);
	void add(const Terms &terms);

	void insert(int index, const Term &term);
	void insert(int index, const Terms &terms);

	size_t size() const;
	const std::vector<Term> &terms() const;

	typedef std::vector<Term>::const_iterator const_iterator;
	typedef std::vector<Term>::iterator iterator;

	const_iterator begin() const;
	const_iterator end() const;

	void remove(const Term &term);
	void remove(const Terms &terms);
	void remove(size_t pos, size_t n = 1);
	void replace(int pos, const Term& term);
	bool discardWhatDoesntContainTheseComponents(	const Terms &terms);
	bool discardWhatDoesContainTheseComponents(		const Terms &terms);
	bool discardWhatDoesContainTheseTerms(			const Terms &terms);
	bool discardWhatIsntTheseTerms(					const Terms &terms, Terms *discarded = nullptr);

	QSet<int> replaceVariableName(const std::string & oldName, const std::string & newName);

	void clear();

	const Term &at(size_t index)								const;
	bool contains(const Term		&	term)					const;
	bool contains(const QString		&	component);
	bool contains(const std::string &	component);
	int	 indexOf(const QString		&	component)				const;

	std::vector<std::string>				asVector()			const;
	std::set<std::string>					asSet()				const;
	std::vector<std::vector<std::string> >	asVectorOfVectors()	const;
	QList<QString>							asQList()			const;
	QList<QList<QString> >					asQListOfQLists()	const;

	Term	sortComponents(const Term &term)	const;
	Terms	sortComponents(const Terms &terms)	const;

	Terms crossCombinations()					const;
	Terms wayCombinations(int ways)				const;
	Terms ffCombinations(const Terms &terms);

	std::string asString() const;

	bool operator==(const Terms &terms) const;
	bool operator!=(const Terms &terms) const;
	const Term& operator[](size_t index) const { return at(index); }

private:

	int		rankOf(const QString &component)						const;
	int		termCompare(const Term& t1, const Term& t2)				const;
	bool	termLessThan(const Term &t1, const Term &t2)			const;
	bool	componentLessThan(const QString &c1, const QString &c2)	const;

	const Terms			*	_parent;
	std::vector<Term>		_terms;
};

#endif // TERMS_H
