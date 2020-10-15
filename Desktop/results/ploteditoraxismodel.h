#ifndef PLOTEDITORAXISMODEL_H
#define PLOTEDITORAXISMODEL_H

#include <QAbstractTableModel>
#include "jsonredirect.h"
#include <vector>

namespace PlotEditor
{

class AxisModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(QString	title		READ title		WRITE setTitle		NOTIFY titleChanged		)
	Q_PROPERTY(QString	type		READ type		WRITE setType		NOTIFY typeChanged		)
	Q_PROPERTY(bool		transposed	READ transposed	WRITE setTransposed	NOTIFY transposedChanged)

public:
	AxisModel(QObject * parent, bool transposed);

	int					rowCount(const QModelIndex &parent = QModelIndex())									const	override;
	int					columnCount(const QModelIndex &parent = QModelIndex())								const	override;
	QVariant			data(const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
	QVariant			headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;
	bool				setData(const QModelIndex &index, const QVariant &value, int role)							override;
	Qt::ItemFlags		flags(const QModelIndex &index)														const	override;

	void				setAxisData(const Json::Value & Axis);
	Json::Value			getAxisData()																		const;

	void				getEntryAndBreaks(size_t & entry, bool & breaks, const QModelIndex & index) const;

	QString				title()			const	{ return _title;		}
	QString				type()			const	{ return _type;			}
	bool				transposed()	const	{ return _transposed;	}

public slots:
	void setTitle(QString title);
	void setType(QString type);
	void setTransposed(bool transposed);

signals:
	void titleChanged(QString title);
	void typeChanged(QString type);
	void transposedChanged(bool transposed);
	void somethingChanged();

private:
	QString					_title,
							_type;
	std::vector<double>		_breaks;
	std::vector<QString>	_labels;
	bool					_transposed = false;
	Json::Value				_axis		= Json::objectValue;

	//add expands as well?
};

}

#endif // PLOTEDITORAXISMODEL_H
