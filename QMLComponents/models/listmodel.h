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
	typedef QMap<QString, RowControls*>							RowControlMap;

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
	RowControls*					getRowControls(const QString& key)								const		{ return _rowControlsMap.value(key);	}
	virtual JASPControl	*			getRowControl(const QString& key, const QString& name)			const;
	virtual bool					addRowControl(const QString& key, JASPControl* control);
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

			bool							_needsSource			= true;
			QMap<QString, RowControls* >	_rowControlsMap;
			QQmlComponent *					_rowComponent			= nullptr;
			Terms::RelatedValuesPerTerm		_rowControlsValues;
			QList<BoundControl *>			_rowControlsConnected;
			QList<int>						_selectedItems;
			QStringList						_columnsUsedForLabels;

private:
			void	_initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap, bool initRowControls = true);
			void	_connectSourceControls(SourceItem* sourceItem);

			JASPListControl*				_listView = nullptr;
			Terms							_terms;
			

};

#endif // LISTMODEL_H
