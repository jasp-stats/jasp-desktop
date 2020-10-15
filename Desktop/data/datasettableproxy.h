#ifndef DATASETTABLEPROXY_H
#define DATASETTABLEPROXY_H

#include <QSortFilterProxyModel>
#include "datasetpackage.h"

class DataSetTableProxy : public QSortFilterProxyModel
{
	Q_OBJECT
	Q_PROPERTY(int proxyParentColumn READ proxyParentColumn WRITE setProxyParentColumn NOTIFY proxyParentColumnChanged)

public:
	explicit				DataSetTableProxy(parIdxType proxyType);

	QModelIndex				mapToSource(	const QModelIndex & proxyIndex)		const	override;
	QModelIndex				mapFromSource(	const QModelIndex & sourceIndex)	const	override;

	int						proxyParentColumn()									const	{ return _proxyParentColumn; }

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
