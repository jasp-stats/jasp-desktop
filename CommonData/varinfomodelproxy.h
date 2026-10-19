#ifndef VarInfoModelProxy_H
#define VarInfoModelProxy_H

#include <QAbstractTableModel>
#include "filtereddata.h"

class VarInfoModelProxy  : public QAbstractTableModel
{
	Q_OBJECT
public:
	enum VarInfoModelProxyRoles {
		NameRole = Qt::UserRole + 1,
		TypeRole,
		ColumnTypeRole,
		ComputedColumnTypeRole,
		IconSourceRole,
		ToolTipRole
	 };
											VarInfoModelProxy(FilteredData * filteredData);

				QVariant					data(			const QModelIndex & index, int role = Qt::DisplayRole)				const	override;
				int							rowCount(		const QModelIndex &parent = QModelIndex())							const	override;
				QHash<int, QByteArray>		roleNames()																			const	override;
				int							columnCount(	const QModelIndex &parent = QModelIndex())							const	override;

private slots:
				void						refresh()	{ beginResetModel(); endResetModel(); }


private:
				FilteredData			*	_filteredData	= nullptr;
};



#endif // VarInfoModelProxy_H
