#ifndef DATASETTABLEPROXY_H
#define DATASETTABLEPROXY_H

#include <QSortFilterProxyModel>
#include "datasetpackage.h"
#include "datasetpackagesubnodemodel.h"

///
/// Makes sure that only a desired subnode of DataSetPackage is passed through
/// It can also be used to filter out rows or columns and even rearrange them (in potential that is)
class DataSetTableProxy : public QSortFilterProxyModel
{
	Q_OBJECT
	
public:
	explicit	DataSetTableProxy(DataSetPackageSubNodeModel * subNodeModel);
		
	DataSetPackageSubNodeModel	*	subNodeModel()	const { return qobject_cast<DataSetPackageSubNodeModel*>(sourceModel()); }
	DataSetBaseNode				*	node()			const { return subNodeModel()->node(); }


	QModelIndex			mapToSource(	const QModelIndex & proxyIndex)				const	override;
	QModelIndex			mapFromSource(	const QModelIndex & sourceIndex)			const	override;

signals:
	void		nodeChanged();
};

#endif // DATASETTABLEPROXY_H
