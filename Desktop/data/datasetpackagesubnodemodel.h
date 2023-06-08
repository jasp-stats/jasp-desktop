#ifndef DATASETPACKAGESUBNODEMODEL_H
#define DATASETPACKAGESUBNODEMODEL_H

#include <QIdentityProxyModel>
#include "datasetbasenode.h"


class DataSetPackageSubNodeModel : public QIdentityProxyModel
{
	Q_OBJECT

public:
	DataSetPackageSubNodeModel(const QString & whatAmI, DataSetBaseNode * node = nullptr);

	QModelIndex			mapToSource(	const QModelIndex & proxyIndex)				const	override;
	QModelIndex			mapFromSource(	const QModelIndex & sourceIndex)			const	override;
	int					rowCount(		const QModelIndex & parent = QModelIndex())	const	override;
	int					columnCount(	const QModelIndex & parent = QModelIndex())	const	override;

	QString				insertColumnSpecial(int column, bool computed, bool R);
	QString				appendColumnSpecial(			bool computed, bool R);

	
	void				selectNode(DataSetBaseNode * node);
	DataSetBaseNode	*	node() const { return _node; }
	
signals:
	void				nodeChanged();
	
public slots:
	void				modelWasReset();

private:
	DataSetBaseNode		*	_node		= nullptr;
	const QString			_whatAmI	= "?";
};

#endif // DATASETPACKAGESUBNODEMODEL_H
