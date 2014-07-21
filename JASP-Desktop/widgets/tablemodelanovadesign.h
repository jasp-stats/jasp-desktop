#ifndef TABLEMODELANOVADESIGN_H
#define TABLEMODELANOVADESIGN_H

#include <QAbstractTableModel>

#include "common.h"

class TableModelAnovaDesign : public QAbstractTableModel
{
	Q_OBJECT
public:
	explicit TableModelAnovaDesign(QObject *parent = 0);

	const QList<int> &design() const;

	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;

signals:

	void designChanged();

public slots:

private:
	QList<int> _design;

};

#endif // TABLEMODELANOVADESIGN_H
