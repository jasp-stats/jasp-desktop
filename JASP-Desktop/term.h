#ifndef TERM_H
#define TERM_H

#include "term.h"

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
