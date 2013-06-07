#ifndef ASSIGNMENTS_H
#define ASSIGNMENTS_H

#include <QWidget>
#include <QAbstractListModel>
#include <QString>
#include <QStringListModel>

#include "../JASP-Common/dataset.h"

#include "../JASP-Common/option.h"
#include "bound.h"

#include "../JASP-Common/optionassignedfields.h"

#include "assignmentitem.h"

using namespace std;

class Assignments : public QWidget, public Bound
{
    Q_OBJECT
public:
	explicit Assignments(DataSet *dataSet, QWidget *parent = 0);
    ~Assignments();

	void assign(QModelIndexList indices);
	void unassign(QModelIndexList indices);

    QAbstractListModel* assignedFieldsModel();
    QAbstractListModel* unassignedFieldsModel();

	void bindTo(Option *option) override;

	//void add(AssignmentItem *assignmentItem);
    
signals:
    
public slots:

private:

    DataSet *m_dataSet;
	std::map<std::string, bool> m_fields;
	QStringListModel *m_unassigned;
	QStringListModel *m_assigned;

	OptionFields *_boundTo;
};

#endif // ASSIGNMENTS_H
