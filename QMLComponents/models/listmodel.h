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
		TypeRole,
		SelectedRole,
		SelectableRole,
		ColumnTypeRole,
		ColumnTypeIconRole,
		ColumnTypeDisabledIconRole,
		RowComponentRole,
		ValueRole,
		VirtualRole,
		DeletableRole
    };
	typedef QMap<QString, RowControls*>							RowControlMap;
	typedef QMap<QString, QMap<QString, Json::Value> >			RowControlsValues;
	typedef QMapIterator<QString, QMap<QString, Json::Value> >	RowControlsValuesIterator;

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
	virtual QString					getItemType(const Term& term)								const		{ return _itemType; }
			void					setItemType(QString type)												{ _itemType = type; }
			void					addControlError(const QString& error)						const;
	virtual void					refresh();
	virtual void					initTerms(const Terms &terms, const RowControlsValues& allValuesMap = RowControlsValues(), bool reInit = false);
			Terms					getSourceTerms();
			ListModel*				getSourceModelOfTerm(const Term& term);
			void					setColumnsUsedForLabels(const QStringList& columns)						{ _columnsUsedForLabels = columns; }
			void					setRowComponent(QQmlComponent* rowComponents);
	virtual void					setUpRowControls();
	const RowControlMap	&			getAllRowControls()											const		{ return _rowControlsMap;				}
	RowControlsValues				getTermsWithComponentValues()								const;
	RowControls*					getRowControls(const QString& key)							const		{ return _rowControlsMap.value(key);	}
	virtual JASPControl	*			getRowControl(const QString& key, const QString& name)		const;
	virtual bool					addRowControl(const QString& key, JASPControl* control);
			QStringList				termsTypes();

	Q_INVOKABLE int					searchTermWith(QString searchString);
	Q_INVOKABLE void				selectItem(int _index, bool _select);
	Q_INVOKABLE void				clearSelectedItems(bool emitSelectedChange = true);
	Q_INVOKABLE void				setSelectedItem(int _index);
	Q_INVOKABLE void				selectAllItems();
	Q_INVOKABLE QList<int>			selectedItems()															{ return _selectedItems; }
    Q_INVOKABLE QList<QString>		selectedItemsTypes()													{ return QList<QString>(_selectedItemsTypes.begin(), _selectedItemsTypes.end()); }


signals:
			void termsChanged();		// Used to signal all kinds of changes in the model. Do not call it directly
			void namesChanged(QMap<QString, QString> map);
			void columnTypeChanged(QString name);
			void labelsChanged(QString columnName, QMap<QString, QString> = {});
			void labelsReordered(QString columnName);
			void columnsChanged(QStringList columns);
			void selectedItemsChanged();
			void oneTermChanged(const QString& oldName, const QString& newName);

public slots:	
	virtual void sourceTermsReset();
	virtual void sourceNamesChanged(QMap<QString, QString> map);
	virtual int  sourceColumnTypeChanged(QString colName);
	virtual bool sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels = {});
	virtual bool sourceLabelsReordered(QString columnName);
	virtual void sourceColumnsChanged(QStringList columns);

			void dataChangedHandler(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles = QVector<int>());

protected:
			void	_setTerms(const Terms& terms);
			void	_setTerms(const Terms& terms, const Terms& parentTerms);
			void	_setTerms(const std::vector<Term>& terms);
			void	_removeTerms(const Terms& terms);
			void	_removeTerm(int index);
			void	_removeTerm(const Term& term);
			void	_removeLastTerm();
			void	_addTerms(const Terms& terms);
			void	_addTerm(const QString& term, bool isUnique = true);
			void	_replaceTerm(int index, const Term& term);
			void	_connectAllSourcesControls();

			QString							_itemType;
			bool							_needsSource			= true;
			QMap<QString, RowControls* >	_rowControlsMap;
			QQmlComponent *					_rowComponent			= nullptr;
			RowControlsValues				_rowControlsValues;
			QList<BoundControl *>			_rowControlsConnected;
			QList<int>						_selectedItems;
			QSet<QString>					_selectedItemsTypes;
			QStringList						_columnsUsedForLabels;

private:
			void	_addSelectedItemType(int _index);
			void	_initTerms(const Terms &terms, const RowControlsValues& allValuesMap, bool initRowControls = true);
			void	_connectSourceControls(SourceItem* sourceItem);

			JASPListControl*				_listView = nullptr;
			Terms							_terms;

};

#endif // LISTMODEL_H
