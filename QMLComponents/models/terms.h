//
// Copyright (C) 2013-2025 University of Amsterdam
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

#include <QString>
#include <QList>

#include "term.h"
#include "controls/jaspcontrol.h"

class VariableInfoConsumer;

///
/// Terms is a list of Term. They are used in VariablesList
/// Some extra functionalities are added to deal with terms with interactions, in order for example to remove all terms that contain some component
/// (a component is most of the time a Variable name, but it can be a factor of level name).
/// Terms may have a parent Terms. This is used mainly in the available Variables List: this list has some variables that can be assigned to another (assigned) list.
/// The variable is then removed from the Available list and added to the assigned list. But if this variable is set back to the available list, it should get the same
/// order as before being set to the assigned list. For this we keep the original terms, and set it as parent of the 'functional' terms of the available list. When a variable
/// is set back to the available list, we can know with the parent terms where it was before being moved.
///
class Terms
{
public:
	///< row-scoped child values keyed by ROW IDENTITY (value + filterId), not by
	///  name: same-name rows behind different filters (the C9 twins) keep their own children
	///  instead of clobbering each other. Read them with relatedValuesForTerm().
	typedef QMap<TermRowKey, QMap<QString, Json::Value> >	RelatedValuesPerTerm;

	///< The related values of one row: exact (value, filterId) hit, else the
	///  (value, -1) entry — the unqualified key the JSON applier writes before the list stamps
	///  its host onto the term (the bind flow) — else empty. An entry belonging to ANOTHER twin
	///  is never handed out: that sharing is exactly what this keying removes.
	static const QMap<QString, Json::Value> &	relatedValuesForTerm(const RelatedValuesPerTerm & values, const Term & term);

	Terms(const QList<QString>								& terms,	Terms *parent = nullptr);
	Terms(const std::vector<std::vector<std::string> >		& terms,	Terms *parent = nullptr);
	Terms(const QList<Term>									& terms,	Terms *parent = nullptr);
	Terms(																Terms *parent = nullptr);
	///< The ONE options-json -> Terms applier: takes the option value as stored
	///  or generated and absorbs every legacy normalization into this chokepoint - unwrapping
	/// `{value, types}` objects, a bare `{value, optionKey?}` Storage object (
	///  types live inside the per-term `{name, filterId?, types}` objects now), a single-string
	///  `types` becoming a one-element array (pre-0.19 files), and the R-syntax-simplified form
	///  of an interaction list WITHOUT row components (bare strings / arrays of strings) becoming
	///  `{keyValue: [components]}` rows. The shape normalization that used to be split across
	///  bind-time helpers + split-parts parsing lives here now: the flags
	///  come from the list (containsInteractions/hasRowComponent), every other caller parses with
	/// defaults. Provenance: the ENGINE shape (RPC/syntax/run requests) carries
	///  no filterId anywhere and parses to all-`-1` terms exactly as before (C15: released files
	///  too, permanently); the STORAGE shape carries it per term (`{name, filterId?, types}`) and
	///  the parsed terms keep those ids - the list's intake (ListModel::_setAllowedType) stamps
	///  only unqualified terms, so loaded stale/foreign ids survive the bind and degrade on the
	///  engine side through the filterById miss into the C7 fallback, never through a restamp.
	Terms(const Json::Value& optionValue, const std::string& keyValue, const std::string& keyLabel, RelatedValuesPerTerm& allControlValues, bool containsInteractions = false, bool hasRowComponent = false);

	void set(const QList<QString>							& terms, bool isUnique = true);
	void set(const std::vector<Term>						& terms, bool isUnique = true);
	void set(const std::vector<std::vector<std::string> >	& terms, bool isUnique = true);
	void set(const QList<Term>								& terms, bool isUnique = true);
	void set(const Terms									& terms, bool isUnique = true);

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
	iterator begin();
	iterator end();

	void remove(const Term &term);
	void remove(const Terms &terms);
	void remove(size_t pos, size_t n = 1);
	void replace(int pos, const Term& term);
	bool discardWhatDoesContainTheseComponents(		const Terms &terms);
	bool discardWhatDoesNotContainTheseComponents(	const Terms &terms);
	bool discardWhatDoesContainTheseTerms(			const Terms &terms);
	///< The NAME-based set partition ("keep only the entries whose value is in `terms`"), used by
	///  the row-level sync flows (ListModelMultiTermsAssigned::initTerms,
	///  ListModelInteractionAssigned::availableTermsResetHandler) where the question is pure set
	///  membership: stamps take no part here, and so does provider provenance - the ORIGIN-
	///  AUTHORITATIVE staleness/prune logic lives in the pool and the diff
	///  (ListModelTermsAvailable), not in Terms.
	bool discardWhatIsntTheseTerms(					const Terms &terms, Terms *discarded = nullptr);

	QSet<int> replaceVariableName(const std::string & oldName, const std::string & newName);

	void clear();

	const Term &at(size_t index)										const;
	Term &at(size_t index);
	///< Term-aware lookups: a lookup term with filterId -1 matches on name alone
	///< (unqualified = context fallback), a stamped one must match name AND filterId — so a list
	///< can hold two same-name terms from different filters and callers still hit the right one.
	///< The QString overloads stay pure name lookups (first term with that name).
	bool containsValue(const Term				&	term)				const;
	bool containsValue(const QString			&	value)				const;
	int	 indexOfLabel(const QString				&	component)			const;
	int	 indexOfValue(const QString				&	value	)			const;
	int	 indexOfValue(const Term				&	value	)			const;

	std::vector<std::string>				valuesAsVector()			const;
	std::vector<std::vector<std::string> >	asVectorOfVectors()			const;
	QStringList								values()					const;
	QStringList								labels()					const;

	Terms crossCombinations()					const;
	Terms wayCombinations(int ways)				const;
	Terms ffCombinations(const Terms &terms);
	Terms combineTerms(JASPControl::CombinationType type);

	bool hasDuplicate() const	{ return _hasDuplicate; }

	bool operator==(const Terms &terms) const;
	bool operator!=(const Terms &terms) const;
	const Term& operator[](size_t index) const { return at(index); }

	bool strictlyEquals(const Terms &terms) const; // Also takes care of the draggable flag
	void setDraggable(bool draggable);
	void setUndraggableTerms(const Terms& undraggableTerms);

	Json::Value types(bool onlyChanged = false, const VariableInfoConsumer* info = nullptr) const;

	Json::Value getValuesOptions() const;
	///< The canonical `{value, types[, optionKey]}` option serialization of these terms:
	///  row objects (keyValue + optional keyLabel + the term's related child values)
	///  when the list has row components or interactions, a single value for isSingleRow, bare
	///  values otherwise. The row assembly is inlined HERE (the separate related-values shuffle
	///  of the mirror era was deleted with it); useValueAndType selects the
	///  `{value, types}` entry shape per row (ComponentsList-style variable lists pass true).
	Json::Value getOptions(const Terms::RelatedValuesPerTerm& relatedValuesPerTerm, const std::string& keyValue, const std::string& keyLabel, bool containsInteractions, bool hasRowComponent, bool isSingleRow, bool useValueAndType = false) const;

	///< The D5.1 Storage serialization of these terms - the FILE format, generated by
	///  BoundControlTerms/ComponentsList/InputList under JsonFormat::Storage. Same tree discipline
	///  as getOptions (rows under keyValue + keyLabel + the term's related child values when the
	///  list has row components or interactions, single term object for isSingleRow, optionKey kept)
	///  but every term is the clean per-term object {name, filterId?, types} (Term::toJsonStorage):
	///  no outer positional `types` array (types belong to the term in typed state) and no
	///  positional sidecar - provenance rides INSIDE each term, which is what makes same-name-
	///  different-filter twins immune to positional drift across a save/reload cycle. Round-trips
	///  losslessly through the Terms(json) applier (pinned by testStorageTermShapeAndRoundTrip).
	Json::Value getStorageOptions(const Terms::RelatedValuesPerTerm& relatedValuesPerTerm, const std::string& keyValue, const std::string& keyLabel, bool containsInteractions, bool hasRowComponent, bool isSingleRow) const;

private:

	int		rankOf(const QString &component)						const;
	int		termCompare(const Term& t1, const Term& t2)				const;
	bool	termLessThan(const Term &t1, const Term &t2)			const;
	bool	componentLessThan(const QString &c1, const QString &c2)	const;
	void	resetValueMap();

	const Terms			*	_parent			= nullptr;
	std::vector<Term>		_terms;
	bool					_hasDuplicate	= false;
	std::map<QString, int>	_valueMap;

};

#endif // TERMS_H
