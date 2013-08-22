#ifndef BOUNDPAIRSTABLE_H
#define BOUNDPAIRSTABLE_H

#include <QTableView>
#include "bound.h"
#include "options/optionfieldpairs.h"
#include "widgets/availablefieldslistview.h"
#include "widgets/assignbutton.h"
#include "dataset.h"

class BoundPairsTable : public QTableView, public Bound
{
	Q_OBJECT
public:
	explicit BoundPairsTable(QWidget *parent = 0);

	virtual void bindTo(Option *option) override;
	
	void setAvailableFieldsListView(AvailableFieldsListView *listView);
	void setAssignButton(AssignButton *button);
	void setDataSet(DataSet *dataSet);

protected:
	void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;

private slots:
	void assign();

private:
	AvailableFieldsListView *_availableFieldsListView;
	AssignButton *_assignButton;
	OptionFieldPairs *_boundTo;

protected:

	class TableModel : public QAbstractTableModel
	{
	public:
		TableModel(QWidget *parent = 0);
		void bindTo(OptionFieldPairs *option);
		int rowCount(const QModelIndex &parent) const override;
		int columnCount(const QModelIndex &parent) const override;
		QVariant data(const QModelIndex &index, int role) const override;
		Qt::ItemFlags flags(const QModelIndex &index) const override;
		void setDataSet(DataSet *dataSet);
		//QVariant headerData(int section, Qt::Orientation orientation, int role) const override;
	private:
		DataSet *_dataSet;
		OptionFieldPairs *_boundTo;
		std::vector<std::pair<std::string, std::string> > _value;
		void pairsChanged();

		QIcon _nominalIcon;
		QIcon _ordinalIcon;
		QIcon _scaleIcon;
	};

	TableModel _model;

};

#endif // BOUNDPAIRSTABLE_H
