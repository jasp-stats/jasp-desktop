#ifndef BOUNDLISTVIEW_H
#define BOUNDLISTVIEW_H

#include "bound.h"

#include <QStringListModel>
#include <QListView>
#include <QIcon>

#include "options/optionfields.h"

#include "availablefieldslistview.h"
#include "assignbutton.h"
#include "dataset.h"

class BoundListView : public QListView, public Bound
{
	Q_OBJECT

public:
	BoundListView(QWidget *parent = 0);

	virtual void bindTo(Option *option) override;

	void setAssignButton(AssignButton *button);
	void setAvailableFieldsListView(AvailableFieldsListView *listView);

	virtual void setDataSet(DataSet *dataSet) override;

protected:
	virtual void focusInEvent(QFocusEvent *event) override;
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;

private slots:
	void assign();

private:

	class AssignedVariables : public QAbstractListModel
	{

	public:
		explicit AssignedVariables(QObject *parent = 0);

		void setDataSet(DataSet *dataSet);

		int rowCount(const QModelIndex &) const override;
		QVariant data(const QModelIndex &index, int role) const override;

		QStringList assigned();
		void setAssigned(QStringList assigned);

	private:
		DataSet *_dataSet;
		QStringList _assignedVariables;

		QIcon _nominalIcon;
		QIcon _ordinalIcon;
		QIcon _scaleIcon;

	};

	void updateList();
	AssignedVariables _listModel;
	OptionFields *_boundTo;

	AvailableFieldsListView *_availableFieldsListView;
	AssignButton *_assignButton;


};

#endif // BOUNDLISTVIEW_H
