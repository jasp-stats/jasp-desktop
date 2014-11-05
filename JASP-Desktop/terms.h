#ifndef TERMS_H
#define TERMS_H

#include <list>

#include "term.h"

class Terms
{
public:
	Terms(const QList<QList<QString> > &terms, Terms *parent = NULL);
	Terms(const QList<QString> &terms, Terms *parent = NULL);
	Terms(const std::vector<std::vector<std::string> > &terms, Terms *parent = NULL);
	Terms(const std::vector<std::string> &terms, Terms *parent = NULL);
	Terms(const QList<Term> &terms, Terms *parent = NULL);
	Terms(Terms *parent = NULL);

	void set(const QList<QList<QString> > &terms);
	void set(const QList<QString> &terms);
	void set(const std::vector<Term> &terms);
	void set(const std::vector<std::string> &terms);
	void set(const std::vector<std::vector<std::string> > &terms);
	void set(const QList<Term> &terms);
	void set(const Terms &terms);
	void set(QByteArray &array);

	void setSortParent(const Terms &parent);

	void add(const Term &term);
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
	void remove(int pos, int n = 1);
	bool discardWhatDoesntContainTheseComponents(const Terms &terms);
	bool discardWhatIsntTheseTerms(const Terms &terms, Terms *discarded = NULL);

	void clear();

	const Term &at(int index) const;
	bool contains(const Term &term) const;
	bool contains(const std::string component);

	std::vector<std::string> asVector() const;
	std::vector<std::vector<std::string> > asVectorOfVectors() const;
	QList<QString> asQList() const;
	QList<QList<QString> > asQListOfQLists() const;

	Term sortComponents(const Term &term) const;
	Terms sortComponents(const Terms &terms) const;

	Terms crossCombinations() const;
	Terms wayCombinations(int ways) const;

	std::string asString() const;

private:

	int rankOf(const QString &component) const;
	int termCompare(const Term& t1, const Term& t2) const;
	bool termLessThan(const Term &t1, const Term &t2) const;
	bool componentLessThan(const QString &c1, const QString &c2) const;

	const Terms *_parent;
	std::vector<Term> _terms;


};

#endif // TERMS_H
