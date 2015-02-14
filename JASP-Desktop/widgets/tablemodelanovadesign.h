#ifndef TABLEMODELANOVADESIGN_H
#define TABLEMODELANOVADESIGN_H

#include "tablemodel.h"
#include "boundmodel.h"
#include "variableinfo.h"

#include "common.h"
#include "terms.h"

#include "options/optionvariables.h"
#include "options/optionstring.h"
#include "options/optionlist.h"
#include "options/optionstable.h"

#include "qutils.h"

typedef QPair<QString, QStringList> Factor;

class TableModelAnovaDesign : public TableModel, public BoundModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	explicit TableModelAnovaDesign(QObject *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;

	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual QVariant data(const QModelIndex &parent = QModelIndex(), int role = Qt::DisplayRole) const OVERRIDE;
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) OVERRIDE;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;

	QList<Factor> design();

signals:
	void designChanging();
	void designChanged();
	void factorAdded(Terms term);
	void factorRemoved(Terms term);

protected:

	void changeRow(int rowNo, std::string value);
	void deleteRow(int row);

	class Row;

	void refresh();

	std::vector<Options *> _groups;

	OptionsTable *_boundTo;

	QList<Row> _rows;


	class Row
	{
	public:

		Row(QString text, bool isHypothetical, int index, int subIndex = -1)
		{
			_text = text;
			_index = index;
			_subIndex = subIndex;
			_isHypothetical = isHypothetical;
		}

		QString text() const
		{
			return _text;
		}

		bool isHeading() const
		{
			return _subIndex == -1;
		}

		bool isHypothetical() const
		{
			return _isHypothetical;
		}

		int index() const
		{
			return _index;
		}

		int subIndex() const
		{
			return _subIndex;
		}

	private:

		QString _text;
		int _index;
		int _subIndex;
		bool _isHypothetical;

	};

};

#endif // TABLEMODELANOVADESIGN_H
