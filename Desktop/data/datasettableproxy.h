#ifndef DATASETTABLEPROXY_H
#define DATASETTABLEPROXY_H

#include <QSortFilterProxyModel>
#include "datasetpackage.h"

///
/// Makes sure that only a desired subnode of DataSetPackage is passed through
/// It can also be used to filter out rows or columns and even though rearrange them (in potential that is)
class DataSetTableProxy : public QSortFilterProxyModel
{
	Q_OBJECT
	Q_PROPERTY(int proxyParentColumn READ proxyParentColumn WRITE setProxyParentColumn NOTIFY proxyParentColumnChanged)

public:
	explicit				DataSetTableProxy(parIdxType proxyType);

	// QModelIndex				mapToSource(	const QModelIndex & proxyIndex)		const	override;
	// QModelIndex				mapFromSource(	const QModelIndex & sourceIndex)	const	override;

	int						proxyParentColumn()									const	{ return _proxyParentColumn; }

	bool					filterAcceptsRow(	int source_row, const QModelIndex & source_parent)	const override;
	bool					filterAcceptsColumn(int source_col, const QModelIndex & source_parent)	const override;

public slots:
	void setProxyParentColumn(int proxyParentColumn);
	void columnsWereRemoved(const QModelIndex & parent, int first, int last) { modelWasReset();	}
	void modelWasReset();

signals:
	void proxyParentColumnChanged();

private:
	parIdxType				_proxyType			= parIdxType::root;
	int						_proxyParentColumn	= 0; //Should work fine for data/root/filter. label will need to change this though, to specify which actual column is being shown.
};

#endif // DATASETTABLEPROXY_H
