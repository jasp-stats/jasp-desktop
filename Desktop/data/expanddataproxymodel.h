#ifndef EXPANDDATAPROXYMODEL_H
#define EXPANDDATAPROXYMODEL_H

#include <QAbstractItemModel>
#include "utils.h"

class DataSetView;

class ExpandDataProxyModel : public QObject
{
public:
	explicit ExpandDataProxyModel(QObject *parent);

	int					rowCount(bool includeVirtuals = true)														const;
	int					columnCount(bool includeVirtuals = true)													const;
	QVariant			headerData(	int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const;
	bool				setData(	int row, int col, const QVariant &value, int role);
	Qt::ItemFlags		flags(int row, int column)																	const;
	QModelIndex			index(int row, int column, const QModelIndex &parent = QModelIndex())						const;
	QVariant			data(int row, int column, int role = Qt::DisplayRole)										const;
	bool				filtered(int row, int column)																const;
	bool				isRowVirtual(int row)																		const;
	bool				isColumnVirtual(int col)																	const;
	bool				expandDataSet()																				const { return _expandDataSet; }
	void				setExpandDataSet(bool expand)																{ _expandDataSet = expand; }

	void				setSourceModel(QAbstractItemModel* model);
	QAbstractItemModel*	sourceModel()																				const { return _sourceModel; }

	void				removeRows(int start, int count);
	void				removeColumns(int start, int count);
	void				removeRow(int row);
	void				removeColumn(int col);
	void				insertRow(int row);
	void				insertColumn(int col);
	QString				insertColumnSpecial(int col, bool computed, bool R);
	void				pasteSpreadsheet(int row, int col, const std::vector<std::vector<QString>> & cells, QStringList newColNames = QStringList());

	int					getRole(const std::string& roleName)														const;

protected:
	void				_setRolenames();
	void				_expandIfNecessary(int row, int col);


	QAbstractItemModel*			_sourceModel			= nullptr;
	bool						_expandDataSet			= false;

	strintmap					_roleNameToRole;

	const int	EXTRA_COLS				= 5;
	const int	EXTRA_ROWS				= 10;
};

#endif // EXPANDDATAPROXYMODEL_H
