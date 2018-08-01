#ifndef LISTMODELTABLE_H
#define LISTMODELTABLE_H

#include "analysis/analysisqmlform.h"
#include "common.h"

#include <QAbstractListModel>
#include <QQuickItem>

class ListModelTable : public QAbstractListModel
{
	Q_OBJECT
public:
	enum TableModelRoles {
        NameRole = Qt::UserRole + 1,
        TypeRole
    };
	
	ListModelTable(AnalysisQMLForm *form, QQuickItem *item);
	virtual QHash<int, QByteArray> roleNames() const OVERRIDE;
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	
	virtual void setUp();
	
	virtual void setTerms(const Terms& terms);
	
protected:
	AnalysisQMLForm* _form;
	QQuickItem* _item;
	
	Terms _terms;
};

#endif // LISTMODELTABLE_H
