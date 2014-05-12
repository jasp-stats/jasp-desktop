#ifndef TABLEMODELVARIABLESLEVELS_H
#define TABLEMODELVARIABLESLEVELS_H

#include <QAbstractListModel>

#include "common.h"
#include "boundmodel.h"
#include "droptarget.h"
#include "options/optionstable.h"
#include "listmodelvariablesavailable.h"
#include "options/optionstring.h"
#include "options/optionlist.h"
#include "tablemodel.h"

 #include "utils.h"

class TableModelVariablesLevels : public TableModel, public BoundModel, public DropTarget
{
	Q_OBJECT
public:
	explicit TableModelVariablesLevels(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;
	virtual QStringList mimeTypes() const OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;

	void setSource(ListModelVariablesAvailable *source);

private:

	class Row
	{
	public:
		Row(QString level)
		{
			_level = level;
			_isLevel = true;
			_option = NULL;
		}

		Row(ColumnInfo variable)
		{
			_variable = variable;
			_isLevel = false;
			_option = NULL;
		}

		Row(OptionList *option)
		{
			_isLevel = false;
			_option = option;
		}

		ColumnInfo variable() const
		{
			return _variable;
		}

		QString name() const
		{
			if (_option != NULL)
				return tq(_option->value());
			else if (_isLevel)
				return _level;
			else
				return _variable.first;
		}

		bool isLevel() const
		{
			return _isLevel;
		}

		bool isOption() const
		{
			return _option != NULL;
		}

		bool isVariable()
		{
			return _option == NULL && _isLevel == false;
		}

		OptionList *option() const
		{
			return _option;
		}

	private:
		bool _isLevel;
		QString _level;
		ColumnInfo _variable;
		OptionList *_option;
	};

	OptionsTable *_boundTo;
	QList<Row> _rows;
	ListModelVariablesAvailable *_source;
	void readFromOption();

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;

};

#endif // TABLEMODELVARIABLESLEVELS_H
