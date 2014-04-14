#include "itemmodelselectitem.h"

#include "utils.h"

using namespace std;

ItemModelSelectItem::ItemModelSelectItem()
{
	_boundTo = NULL;
}

void ItemModelSelectItem::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionList *>(option);
}

int ItemModelSelectItem::rowCount(const QModelIndex &parent) const
{
	if (_boundTo == NULL)
		return 0;

	return _boundTo->options().size();
}

QVariant ItemModelSelectItem::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL || role != Qt::DisplayRole)
		return QVariant();

	return tq(_boundTo->options().at(index.row()));
}

void ItemModelSelectItem::setValueByIndex(int index)
{
	if (_boundTo == NULL)
		return;

	_boundTo->setValue(_boundTo->options().at(index));
}

int ItemModelSelectItem::valueIndex() const
{
	if (_boundTo == NULL)
		return 0;

	string selected = _boundTo->value();
	vector<string> options = _boundTo->options();

	for (int i = 0; i < options.size(); i++)
	{
		string value = options.at(i);
		if (value == selected)
			return i;
	}

	return 0;
}

void ItemModelSelectItem::setValue(QString value)
{
	if (_boundTo == NULL)
		return;

	_boundTo->setValue(fq(value));
}

QString ItemModelSelectItem::value() const
{
	if (_boundTo == NULL)
		return "";

	return tq(_boundTo->value());
}
