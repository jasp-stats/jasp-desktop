#ifndef DATASETPACKAGESUBNODEMODEL_H
#define DATASETPACKAGESUBNODEMODEL_H

#include <QIdentityProxyModel>
#include "datasetbasenode.h"


class DataSetPackageSubNodeModel : public QIdentityProxyModel
{
	Q_OBJECT

public:
	DataSetPackageSubNodeModel(DataSetBaseNode * node = nullptr);

	QModelIndex			mapToSource(	const QModelIndex & proxyIndex)				const	override;
	QModelIndex			mapFromSource(	const QModelIndex & sourceIndex)			const	override;

	
	void				selectNode(DataSetBaseNode * node);
	DataSetBaseNode	*	node() const { return _node; }
	
signals:
	void				nodeChanged();
	
public slots:
	void				modelWasReset();

private:
	DataSetBaseNode		*	_node = nullptr;
};

#endif // DATASETPACKAGESUBNODEMODEL_H
