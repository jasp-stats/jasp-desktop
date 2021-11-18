#ifndef DATASETPACKAGESUBNODEMODEL_H
#define DATASETPACKAGESUBNODEMODEL_H

#include <QIdentityProxyModel>
#include "datasetdefinitions.h"

class DataSetPackageSubNodeModel : public QIdentityProxyModel
{
	Q_OBJECT
	Q_PROPERTY(int proxyParentColumn READ proxyParentColumn WRITE setProxyParentColumn NOTIFY proxyParentColumnChanged)

public:
	DataSetPackageSubNodeModel(parIdxType proxyType = parIdxType::dataRoot, int proxyParentColumn = 0);

	QModelIndex			mapToSource(	const QModelIndex & proxyIndex)				const	override;
	QModelIndex			mapFromSource(	const QModelIndex & sourceIndex)			const	override;
	/*int					rowCount(		const QModelIndex &parent = QModelIndex())	const	override;
	int					columnCount(	const QModelIndex &parent = QModelIndex())	const	override;
	QModelIndex			parent(			const QModelIndex & index)					const	override;
	QModelIndex			index(int row, int column, const QModelIndex &parent)		const	override;*/


	int					proxyParentColumn() const;
	void				setProxyParentColumn(int newProxyParentColumn);

signals:
	void				proxyParentColumnChanged();


public slots:
	void				modelWasReset();

private:
	parIdxType			_proxyType			;
	int					_proxyParentColumn	;
};

#endif // DATASETPACKAGESUBNODEMODEL_H
