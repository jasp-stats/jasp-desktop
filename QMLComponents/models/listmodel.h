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

#ifndef LISTMODEL_H
#define LISTMODEL_H

#include <QAbstractListModel>
#include <QQmlComponent>

#include "common.h"
#include "terms.h"
#include <json/json.h>
#include "variableinfo.h"

class JASPListControl;
class RowControls;
class JASPControl;
class BoundControl;
class SourceItem;

class ListModel : public QAbstractTableModel, public VariableInfoConsumer
{
	Q_OBJECT

public:
	enum ListModelRoles
	{
        NameRole = Qt::UserRole + 1,
		ValueRole,
		InfoRole,
		TypeRole,
		SelectedRole,
		SelectableRole,
		ColumnTypeRole,
		ColumnPreviewRole,
		ColumnRealTypeRole,
		ColumnTypeIconRole,
		ColumnDescriptionRole,
		ColumnTypeDisabledIconRole,
		RowComponentRole,
		VirtualRole,
		DeletableRole
    };
	///< mds-plan D6.1: row controls keyed by ROW IDENTITY (value + filterId), not by name, so
	///  same-name twin rows behind different filters each get and keep their own controls.
	typedef QMap<TermRowKey, RowControls*>						RowControlMap;

	ListModel(JASPListControl* listView);
	
			QHash<int, QByteArray>	roleNames()													const override;
			int						rowCount(const QModelIndex &parent = QModelIndex())			const override;
			int						columnCount(const QModelIndex &parent = QModelIndex())		const override { return 1; }
			QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;

			JASPListControl*		listView()													const		{ return _listView; }
			const QString &			name() const;
			Terms					termsEx(const QStringList& what);
			const Terms &			terms()														const		{ return _terms;	}
	virtual Terms					filterTerms(const Terms& terms, const QStringList& filters);
			bool					needsSource()												const		{ return _needsSource;			}
			void					setNeedsSource(bool needs)												{ _needsSource = needs;			}
			void					addControlError(const QString& error)						const;
	virtual void					refresh();
            virtual void			initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap = {});
			Terms					getSourceTerms();
			void					setColumnsUsedForLabels(const QStringList& columns)						{ _columnsUsedForLabels = columns; }
			void					setRowComponent(QQmlComponent* rowComponents);
	virtual void					setUpRowControls(int startRow = 0, bool onlyRemove = false);
	const RowControlMap	&			getAllRowControls()												const		{ return _rowControlsMap;				}
	Terms::RelatedValuesPerTerm		getTermsWithComponentValues()									const;
	///< Exact row-controls access by row identity (mds-plan D6.1). The QString overload resolves
	///< the name through the model (first row term with that name) - the legacy name-only API,
	///< deterministic for twins now.
	RowControls*					getRowControls(const TermRowKey& key)							const		{ return _rowControlsMap.value(key);	}
	RowControls*					getRowControls(const QString& key)							const;
	virtual JASPControl	*			getRowControl(const QString& key, const QString& name)			const;
	///< Exact (name, filterId) row-control lookup (mds-plan D6.1): every C++ caller that holds the
	///< row's term uses this, so a twin never resolves to the other row's control.
	virtual JASPControl	*			getRowControl(const TermRowKey& key, const QString& name)			const;
	///< Registration of a row control into the RowControls that is being (re)built (mds-plan
	///< D6.1): the key is the creating row's identity (value + filterId, or the tableview's
	///< per-row index string with filterId -1), never a bare name - name-based registration
	///< would put a twin row's controls into the first row's set.
	virtual bool					addRowControl(const TermRowKey& key, JASPControl* control);
			QStringList				allLevels(const Terms& terms)									const;
			void					setVariableType(int index, columnType type);
			columnType				getVariableType(	const QString& name)						const;
			QString					getVariableDescription(	const QString& name)					const;
			Json::Value				getVariableTypes(bool onlyChanged = false)						const;
			Json::Value				getVariableTypes(const Terms& terms, bool onlyChanged = false)	const;
			columnType				getVariableRealType(const QString& name)						const;
			QString					getVariablePreview(	const QString& name)						const;
			QStringList				getUsedTypes()													const;

			Terms					checkTermsTypes(const Terms& terms)								const;
			Terms					checkTermsTypes(const std::vector<Term>& terms)					const;
			void					checkTermsTypes();
			///< mds-plan post (ORIGIN-AUTHORITATIVE participation rule): a term follows a provider
			///  update iff it is a reference into the provider that changed - its stamp equals
			///  `filterIdToUpdate`, the filterId the affected terms CURRENTLY carry (the old
			///  selection id on a host switch, the unchanged selection id on a data change, -1
			///  for a hostless/default-provider pool). Plain equality, no special cases: a -1
			///  term participates exactly when the key is -1 (a hostless pool's update - the
			///  default provider IS that pool's provider, and the replacement it delivers is
			///  -1-stamped too, so -1 terms never change identity through this); on a qualified
			///  update a -1 term is a reference into the provider WITHOUT host selection, which
			///  resolves at use time through the C13 chain and is governed by THAT provider, so
			///  it is never touched here. A foreign stamp (a typed copy picked/bound through
			///  another pool, mds-plan D6.2) belongs to that other provider and keeps its
			///  provenance.
			static bool			termFollowsProviderChange(int termFilterId, int filterIdToUpdate)
			{
				return termFilterId == filterIdToUpdate;
			}
			///< mds-plan post: ONE reconcile routine for a provider update (the signal-driven
			///  successor of the deleted restamp walk, HostFilterSelection::restampValuesUnderHost
			///  / restampTermsFilterId). `filterIdToUpdate` is the participation key (see
			///  termFollowsProviderChange); `newUniverse` is the pool's rebuilt universe, whose
			///  terms are birth-stamped by the pool (FIX 1). A participating term whose NAME the
			///  universe still serves (NAME-ONLY comparison - the provenance pre-selection
			///  already happened via the key, so the value-only lookup is deliberate) is
			///  REPLACED BY the delivered universe term - stamp and fresh type ride the
			///  replacement, never an in-place re-point (the same identity-change class as a
			///  rename, so the row maps re-key exactly like the rename flow,
			///  ListModelTermsAssigned::changeTerm); a vanished name is collected into the
			///  returned Terms for the caller to remove with its own family machinery. Emits
			///  dataChanged for the replaced rows (which folds into termsChanged ->
			///  usedVariablesChanged), so the update is never silent.
			///  Virtual: the multi-column and interaction families reconcile their own shapes (the
			///  tuples / the components), the base version is the single-column rule.
			virtual Terms		reconcileProviderChange(int filterIdToUpdate, const Terms &newUniverse);
			///< Introspection (QML tests, mds-plan D2.5): "name@filterId" per model term - the
			///< typed proof that a host switch restamped exactly the controls of the host that
			///< moved. Empty filterId part (-1) means unqualified (no explicit host).
			Q_INVOKABLE QStringList	termProvenanceForTest() const;
	virtual Terms					termsFromIndexes(	const QList<int>& indexes)					const;
	virtual QList<int>				indexesFromTerms(	const Terms		& terms)					const;


	Q_INVOKABLE int					searchTermWith(QString searchString);
	Q_INVOKABLE void				selectItem(int _index, bool _select);
	Q_INVOKABLE void				clearSelectedItems(bool emitSelectedChange = true);
	Q_INVOKABLE void				setSelectedItem(int _index);
	Q_INVOKABLE void				setSelectedItemWithName(QString name);
	Q_INVOKABLE void				selectAllItems();
	Q_INVOKABLE QList<int>			selectedItems()															{ return _selectedItems; }

				void				cleanUp();
signals:
			void termsChanged();		// Used to signal all kinds of changes in the model. Do not call it directly
			void variableNamesChanged(QMap<QString, QString> map);
			void labelsChanged(QString columnName, QMap<QString, QString> = {});
			void labelsReordered(QString columnName);
			void filterChanged();
			void variablesChanged(QStringList columns);
			void selectedItemsChanged();
			void keyTermChanged(const QString& oldName, const QString& newName);
			void variableTypeChanged(QString columnName, columnType columnType);

public slots:	
	virtual void sourceTermsReset();
	virtual void sourceVariableNamesChanged(QMap<QString, QString> map);
	virtual bool sourceVariableTypeChanged(QString columnName, columnType colType);
	virtual bool sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels = {});
	virtual bool sourceLabelsReordered(QString columnName);
	virtual void sourceVariablesChanged(QStringList columns);

			void dataChangedHandler(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles = QVector<int>());

protected:
			///< The one init write path; protected like the other _-helpers below (subclasses and
			///  test harnesses seed models without the listView-dependent source connecting).
			void	_initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap, bool initRowControls = true);
			void	_setTerms(const Terms& terms);
			void	_setTerms(const Terms& terms, const Terms& parentTerms);
			void	_removeTerms(const Terms& terms);
			void	_removeTerm(int index);
			void	_removeTerm(const Term& term);
			void	_removeLastTerm();
			void	_addTerms(const Terms& terms);
			void	_addTerm(const Term& term, bool isUnique = true);
			void	_replaceTerm(int index, const Term& term);
			void	_connectAllSourcesControls();
			Term	_checkTermType(const Term& terms)					const;
			void	_setAllowedType(Term& term)							const;
			///< The filter id of the selection this list's variables resolve through (mds-plan C9),
			///< -1 when there is no explicit selection host — see _setAllowedType.
			int		_currentSelectionFilterId()							const;

			bool							_needsSource			= true;
			RowControlMap						_rowControlsMap;			//mds-plan D6.1: keyed by row identity (TermRowKey), see getAllRowControls
			QQmlComponent *					_rowComponent			= nullptr;
			Terms::RelatedValuesPerTerm		_rowControlsValues;
			QList<BoundControl *>			_rowControlsConnected;
			QList<int>						_selectedItems;
			QStringList						_columnsUsedForLabels;

private:
			void	_connectSourceControls(SourceItem* sourceItem);

			JASPListControl*				_listView = nullptr;
			Terms							_terms;
			

};

#endif // LISTMODEL_H
