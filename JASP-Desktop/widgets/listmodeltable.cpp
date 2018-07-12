#include "listmodeltable.h"

#include <QQmlProperty>

ListModelTable::ListModelTable(AnalysisQMLForm *form, QQuickItem *item)
	: QAbstractListModel(form)
	, _item(item)
	, _form(form)
{
	
}

QHash<int, QByteArray> ListModelTable::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[TypeRole] = "type";
	roles[NameRole] = "name";
	return roles;
}

int ListModelTable::rowCount(const QModelIndex &parent) const
{
	return _terms.size();
}

QVariant ListModelTable::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		Term term = _terms.at(row);
		return QVariant(term.asQString());		
	}
	
	return QVariant();
}

void ListModelTable::setUp()
{
	QQmlProperty(_item, "model").write(QVariant::fromValue(this));
}

void ListModelTable::setTerms(const Terms &terms)
{
	beginResetModel();
	_terms.set(terms.terms());
	endResetModel();
}
