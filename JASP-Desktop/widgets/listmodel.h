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

#include "common.h"
#include "analysis/options/variableinfo.h"
#include "analysis/options/terms.h"

class QMLListView;
class ListModelExtraControls;

class ListModel : public QAbstractTableModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	enum ListModelRoles
	{
        NameRole = Qt::UserRole + 1,
		TypeRole,
		ColumnTypeRole,
		ExtraColumnsRole
    };

	ListModel(QMLListView* listView);
	
			QHash<int, QByteArray>	roleNames()													const override;
			int						rowCount(const QModelIndex &parent = QModelIndex())			const override;
			int						columnCount(const QModelIndex &parent = QModelIndex())		const override { return 1; }
			QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual void					endResetModel();

			QMLListView*			listView() const								{ return _listView; }
			const QString &			name() const;
	virtual const Terms &			terms(const QString& what = QString())			{ return _terms; }
			bool					areTermsVariables() const						{ return _areTermsVariables; }
			bool					areTermsInteractions() const					{ return _areTermsInteractions; }
	virtual QString					getItemType(const Term& term) const				{ return _itemType; }
			void					setTermsAreVariables(bool areVariables)			{ _areTermsVariables = areVariables; }
			void					setTermsAreInteractions(bool interactions)		{ _areTermsInteractions = interactions; }
			void					setItemType(QString type)						{ _itemType = type; }
			void					addError(const QString& error) const;
	virtual void					refresh();
	virtual void					initTerms(const Terms &terms);
	virtual Terms					getSourceTerms();
	QMap<ListModel*, Terms> 		getSourceTermsPerModel();
	
	void							addExtraControls(const QVector<QMap<QString, QVariant> >& extraControlColumns);
	ListModelExtraControls*			getExtraControlModel(QString colName)			{ return _extraControlsModels[colName]; }




signals:
			void modelChanged(Terms* added = nullptr, Terms* removed = nullptr);

public slots:	
	virtual void sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved);

protected:
	void addExtraControlModels();
	void initExtraControlTerms();

private:
	void _initExtraControlTerms();


protected:
	QMLListView*	_listView = nullptr;
	QString			_itemType;
	Terms			_terms;
	bool			_areTermsVariables;
	bool			_areTermsInteractions = false;

	QVector<QMap<QString, QVariant> >		_extraControlsDefinitions;
	QMap<QString, ListModelExtraControls* > _extraControlsModels;
	QMap<int, QString>						_rowNameMap;
	QMap<QString, ListModelExtraControls* > _modelCache;

};

#endif // LISTMODEL_H
