#include "assignments.h"

#include <QDebug>
#include "boost/foreach.hpp"

using namespace std;

Assignments::Assignments(DataSet *dataSet, QWidget *parent) :
    QWidget(parent)
{
    m_dataSet = dataSet;

	vector<string> headers = dataSet->columnHeaders();

	QStringList unassigned;

	BOOST_FOREACH (string value, headers)
	{
		unassigned.append(QString::fromUtf8(value.c_str(), value.length()));
		m_fields.insert(pair<string, bool>(value, false));
	}

	m_unassigned = new QStringListModel(unassigned, this);
    m_assigned = new QStringListModel(this);

}

Assignments::~Assignments()
{

}

void Assignments::assign(QModelIndexList indices)
{
	BOOST_FOREACH(QModelIndex &index, indices)
    {
        QString field = m_unassigned->stringList().at(index.row());
		QByteArray utf8 = field.toUtf8();
		string str = string(utf8.constData(), utf8.length());
		m_fields[str] = true;
    }

	QStringList assignedStringList;
	QStringList unassignedStringList;
	vector<string> assigned;

	typedef pair<string, bool> pair;
	BOOST_FOREACH(pair p, m_fields)
	{
		QString field = QString::fromUtf8(p.first.c_str(), p.first.length());

		if (p.second)
		{
			assignedStringList.append(field);
			assigned.push_back(p.first);
		}
		else
			unassignedStringList.append(field);
	}

	m_assigned->setStringList(assignedStringList);
	m_unassigned->setStringList(unassignedStringList);

	if (_boundTo != NULL)
		_boundTo->setValue(assigned);
}

void Assignments::unassign(QModelIndexList indices)
{
	BOOST_FOREACH(QModelIndex &index, indices)
	{
		QString field = m_unassigned->stringList().at(index.row());
		QByteArray utf8 = field.toUtf8();
		string str = string(utf8.constData(), utf8.length());
		m_fields[str] = false;
	}

	QStringList assignedStringList;
	QStringList unassignedStringList;
	vector<string> assigned;

	typedef pair<string, bool> pair;
	BOOST_FOREACH(pair p, m_fields)
	{
		QString field = QString::fromUtf8(p.first.c_str(), p.first.length());

		if (p.second)
		{
			assignedStringList.append(field);
			assigned.push_back(p.first);
		}
		else
			unassignedStringList.append(field);
	}

	m_assigned->setStringList(assignedStringList);
	m_unassigned->setStringList(unassignedStringList);

	if (_boundTo != NULL)
		_boundTo->setValue(assigned);
}

QAbstractListModel *Assignments::assignedFieldsModel()
{
    return m_assigned;
}

QAbstractListModel *Assignments::unassignedFieldsModel()
{
	return m_unassigned;
}

void Assignments::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFields*>(option);

	if (_boundTo == NULL)
		qDebug() << "could not bind to OptionAssignedFields in assignments.cpp";
}

/*void Assignments::add(AssignmentItem *assignmentsItem)
{
}*/


