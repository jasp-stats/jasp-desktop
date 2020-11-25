#ifndef PLOTEDITORAXISMODEL_H
#define PLOTEDITORAXISMODEL_H

#include <QAbstractTableModel>
#include "jsonredirect.h"
#include <vector>
#include <cmath>

namespace PlotEditor
{

class AxisModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(QString	title		READ title			WRITE setTitle			NOTIFY titleChanged				)
	Q_PROPERTY(QString	titleType	READ titleType		WRITE setTitleType		NOTIFY titleTypeChanged			)
	Q_PROPERTY(QString	type		READ type			WRITE setType			NOTIFY typeChanged				)
	Q_PROPERTY(bool		vertical	READ vertical		WRITE setVertical		NOTIFY verticalChanged			)

	Q_PROPERTY(QString	breaksType	READ breaksType		WRITE setBreaksType		NOTIFY rangeChanged				)
	Q_PROPERTY(double	from		READ from			WRITE setFrom			NOTIFY rangeChanged				)
	Q_PROPERTY(double	to			READ to				WRITE setTo				NOTIFY rangeChanged				)
	Q_PROPERTY(double	steps		READ steps			WRITE setSteps			NOTIFY rangeChanged				)

	Q_PROPERTY(QString	limitsType	READ limitsType		WRITE setLimitsType		NOTIFY limitsChanged			)
	Q_PROPERTY(double	limitLower	READ lower			WRITE setLower			NOTIFY limitsChanged			)
	Q_PROPERTY(double	limitUpper	READ upper			WRITE setUpper			NOTIFY limitsChanged			)

public:
	AxisModel(QObject * parent, bool vertical) : QAbstractTableModel(parent), _vertical(vertical)
	{ }

	enum class	specialRoles
	{
		insertLeft	= Qt::UserRole,
		insertRight,
		deleteBreak
	};

	int						rowCount(	const QModelIndex &parent = QModelIndex())								const	override;
	int						columnCount(const QModelIndex &parent = QModelIndex())								const	override;
	QVariant				data(		const QModelIndex &index, int role = Qt::DisplayRole)					const	override;
	bool					setData(	const QModelIndex &index, const QVariant &value, int role)						override;
	Qt::ItemFlags			flags(		const QModelIndex &index)												const	override;
	QHash<int, QByteArray>	roleNames()																			const	override;
	QVariant				headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;


	void				setAxisData(const Json::Value & Axis);
	Json::Value			getAxisData()																		const;

	void				getEntryAndBreaks(size_t & entry, bool & breaks, const QModelIndex & index) const;

	QString				title()			const	{ return _title;		}
	QString				type()			const	{ return _type;			}
	QString				titleType()		const	{ return _titleType;	}
	QString				breaksType()	const	{ return _breaksType;	}
	bool				vertical()	const	{ return _vertical;	}

	double				from()			const	{ return _range.size() > 0 ? _range[0] : NAN;		}
	double				to()			const	{ return _range.size() > 1 ? _range[1] : NAN;		}
	double				steps()			const	{ return _range.size() > 2 ? _range[2] : NAN;		}

	QString				limitsType()	const	{ return _limitsType;	}
	double				lower()			const	{ return _limits.size() > 0 ? _limits[0] : NAN;	}
	double				upper()			const	{ return _limits.size() > 1 ? _limits[1] : NAN;	}

	bool				hasBreaks()		const	{ return _breaks.size() > 0; }

public slots:
	void setTitle(		QString title);
	void setTitleType(	QString title);
	void setType(		QString type);
	void setVertical(bool vertical);
	void insertBreak(const QModelIndex &index, const size_t column, const bool left);
	void deleteBreak(const QModelIndex &index, const size_t column);

	void setBreaksType(const QString breaksType);
	void setRange(	const double value, const size_t idx);
	void setFrom(	const double from)			{ setRange(from,	0);	}
	void setTo(		const double to)			{ setRange(to,		1);	}
	void setSteps(	const double steps)			{ setRange(steps,	2);	}

	void setLimitsType(const QString limitsType);
	void setLimits(	const double value, const size_t idx);
	void setLower(	const double lower)			{ setLimits(lower,	0);	}
	void setUpper(	const double upper)			{ setLimits(upper,	1);	}

signals:
	void titleChanged(		QString title);
	void titleTypeChanged(	QString title);
	void typeChanged(		QString type);
	void verticalChanged(	bool	transposed);
	void rangeChanged();
	void limitsChanged();
	void somethingChanged();

private:
	QString					_title,
							_type,
							_titleType,
							_breaksType,
							_limitsType;
	std::vector<double>		_range,
							_breaks,
							_limits;
	std::vector<QString>	_labels;
	bool					_vertical	= false;
	Json::Value				_axis		= Json::objectValue;

	//add expands as well?

	void					fillFromJSON(std::vector<double>	&obj,	Json::Value value);
	void					fillFromJSON(std::vector<QString>	&obj,	Json::Value value);

};

}

#endif // PLOTEDITORAXISMODEL_H
