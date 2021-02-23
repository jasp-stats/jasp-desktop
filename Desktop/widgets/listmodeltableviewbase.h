//
// Copyright (C) 2013-2019 University of Amsterdam
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

#ifndef LISTMODELTABLEVIEWBASE_H
#define LISTMODELTABLEVIEWBASE_H

#include "listmodel.h"
#include "common.h"

class TableViewBase;

///This class makes sure the roles that DataSetView expects are implemented and returned.
class ListModelTableViewBase : public ListModel
{
	Q_OBJECT

public:
	struct TableTerms
	{
		QVector<QVector<QVariant> >	values;
		QStringList					rowNames,
									colNames,
									variables;
		QString						colName,
									extraCol,
									filter;
		QVector<int>				rowIndices;

		TableTerms() {}

		void clear()
		{
			values.clear();
			rowNames.clear();
			colNames.clear();
			variables.clear();
			colName.clear();
			extraCol.clear();
			filter.clear();
		}
	};


	enum class	specialRoles		{ active = Qt::UserRole, lines, maxColString, maxRowHeaderString, itemInputType };

	explicit						ListModelTableViewBase(TableViewBase * tableView);

	QHash<int, QByteArray>			roleNames() const override;

				int					rowCount(	const QModelIndex & = QModelIndex())									const	override { return _tableTerms.rowNames.length();	}
				int					columnCount(const QModelIndex & = QModelIndex())									const	override { return _tableTerms.colNames.length();	}
				int					variableCount()																		const			 { return _tableTerms.variables.length();	}
				QVariant			data(		const QModelIndex &index, int role = Qt::DisplayRole)					const	override;
				Qt::ItemFlags		flags(		const QModelIndex &index)												const	override;
				QVariant			headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;

				void				sourceTermsReset()																			override;

	virtual		int					getMaximumColumnWidthInCharacters(size_t columnIndex)								const;
				QString				getMaximumRowHeaderString()															const;

	virtual		void				initTableTerms(const TableTerms& terms);
				void				addColumn(					bool emitStuff = true);
				void				removeColumn(size_t index,	bool emitStuff = true);
				void				addRow(						bool emitStuff = true);
				void				removeRow(size_t index,		bool emitStuff = true);
	virtual		void				reset();
	virtual		void				setup() {}
	virtual		void				itemChanged(int column, int row, QVariant value, QString type);
	virtual		void				refreshModel()																			{ return ListModel::refresh(); }
	virtual		QString				getDefaultColName(size_t index)										const;
	virtual		QString				getDefaultRowName(size_t index)										const				{ return tr("Row %1").arg(index); }
	virtual		bool				isEditable(const QModelIndex &)										const				{ return true; }
	virtual		QString				getItemInputType(const QModelIndex &)								const;

	const		TableTerms	&		tableTerms()														const				{ return _tableTerms; }
				Terms				filterTerms(const Terms& terms, const QStringList& filters)					override;


				void				runRScript(		const QString & script);
	virtual		void				rScriptDoneHandler(const QString & result) { throw std::runtime_error("runRScript done but handler not implemented!\nImplement an override for RScriptDoneHandler and usesRScript\nResult was: "+result.toStdString()); }

				bool				valueOk(QVariant value);
	virtual		bool				isRCodeColumn(int)													const				{ return false; }


				JASPControl*		getRowControl(const QString& key, const QString& name)				const	override;
				bool				addRowControl(const QString& key, JASPControl* control)						override;

signals:
	void columnCountChanged();
	void rowCountChanged();
	void variableCountChanged();
	void itemChangedSignal(int column, int row, double value);

public slots:
	virtual void initialValuesChanged() {}

protected slots:
	void formulaCheckSucceededSlot();

protected:
	TableViewBase			*	_tableView		= nullptr;
	TableTerms					_tableTerms;

	const size_t				_maxColumn		= 10,
								_maxRow			= 100;
	int							_rowSelected	= -1;
	bool						_keepRowsOnReset = true,
								_keepColsOnReset = false;

	QMap<QString, QMap<QString, JASPControl*> >	_itemControls;
};

#endif // LISTMODELTABLEVIEWBASE_H
