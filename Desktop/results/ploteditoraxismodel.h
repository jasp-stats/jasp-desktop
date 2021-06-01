#ifndef PLOTEDITORAXISMODEL_H
#define PLOTEDITORAXISMODEL_H

#include <QAbstractTableModel>
#include "jsonredirect.h"
#include <vector>
#include <cmath>

namespace PlotEditor
{

class PlotEditorModel;

class AxisModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_ENUMS(TitleType)
	Q_ENUMS(BreaksType)
	Q_ENUMS(LimitsType)

	Q_PROPERTY(QString		title		READ title			WRITE setTitle			NOTIFY titleChanged				)
	Q_PROPERTY(TitleType	titleType	READ titleType		WRITE setTitleType		NOTIFY titleTypeChanged			)
	Q_PROPERTY(QString		type		READ type			WRITE setType			NOTIFY typeChanged				)
	Q_PROPERTY(bool			vertical	READ vertical		WRITE setVertical		NOTIFY verticalChanged			)

	Q_PROPERTY(BreaksType	breaksType	READ breaksType		WRITE setBreaksType		NOTIFY rangeChanged				)
	Q_PROPERTY(double		from		READ from			WRITE setFrom			NOTIFY rangeChanged				)
	Q_PROPERTY(double		to			READ to				WRITE setTo				NOTIFY rangeChanged				)
	Q_PROPERTY(double		steps		READ steps			WRITE setSteps			NOTIFY rangeChanged				)

	Q_PROPERTY(LimitsType	limitsType	READ limitsType		WRITE setLimitsType		NOTIFY limitsChanged			)
	Q_PROPERTY(double		limitLower	READ lower			WRITE setLower			NOTIFY limitsChanged			)
	Q_PROPERTY(double		limitUpper	READ upper			WRITE setUpper			NOTIFY limitsChanged			)

	Q_PROPERTY(bool			continuous	READ continuous								NOTIFY continuousChanged		)
	//Q_PROPERTY(bool		hasBreaks	READ hasBreaks								NOTIFY hasBreaksChanged			) //Is a bit misleading as apparently you can have breaks without _breaks containing anything?

public:
	AxisModel(PlotEditorModel * parent, bool vertical);

	enum class	specialRoles
	{
		insertLeft	= Qt::UserRole,
		insertRight,
		deleteBreak
	};

	enum class TitleType  { TitleCharacter, TitleExpression, TitleLaTeX, TitleNull };
	enum class BreaksType { BreaksRange, BreaksManual, BreaksNull};
	enum class LimitsType { LimitsData,  LimitsBreaks, LimitsManual };

	int						rowCount(	const QModelIndex &parent = QModelIndex())								const	override;
	int						columnCount(const QModelIndex &parent = QModelIndex())								const	override;
	QVariant				data(		const QModelIndex &index, int role = Qt::DisplayRole)					const	override;
	bool					setData(	const QModelIndex &index, const QVariant &value, int role)						override;
	Qt::ItemFlags			flags(		const QModelIndex &index)												const	override;
	QHash<int, QByteArray>	roleNames()																			const	override;
	QVariant				headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;
	bool					insertColumns(int column, int count, const QModelIndex &parent = QModelIndex())				override;
	bool					removeColumns(int column, int count, const QModelIndex &parent = QModelIndex())				override;

	void				setAxisData(const Json::Value & Axis);
	Json::Value			getAxisData()																		const;

	void				getEntryAndBreaks(size_t & entry, bool & breaks, const QModelIndex & index) const;

	QString				title()			const	{ return _title;		}
	QString				type()			const	{ return _type;			}
	TitleType			titleType()		const	{ return _titleType;	}
	BreaksType			breaksType()	const	{ return _breaksType;	}
	bool				vertical()		const	{ return _vertical;		}

	double				from()			const	{ return _range.size() > 0 ? _range[0] : NAN;		}
	double				to()			const	{ return _range.size() > 1 ? _range[1] : NAN;		}
	double				steps()			const	{ return _range.size() > 2 ? _range[2] : NAN;		}

	LimitsType			limitsType()	const	{ return _limitsType;	}
	double				lower()			const	{ return _limits.size() > 0 ? _limits[0] : NAN;	}
	double				upper()			const	{ return _limits.size() > 1 ? _limits[1] : NAN;	}

	bool				hasBreaks()		const	{ return _breaks.size() > 0; }

	bool				continuous()	const	{ return _continuous;	}

	void				simplifyLimitsType();

public slots:
	void setTitle(		QString title);
	void setTitleType(	TitleType title);
	void setType(		QString type);
	void setVertical(bool vertical);
	void insertBreak(const QModelIndex &index, const size_t column, const bool left);
	void deleteBreak(const QModelIndex &index, const size_t column);

	void setBreaksType(const BreaksType breaksType);
	void setRange(	const double value, const size_t idx);
	void setFrom(	const double from);
	void setTo(		const double to);
	void setSteps(	const double steps)			{ setRange(steps,	2);	}

	void setLimitsType(const LimitsType limitsType);
	void setLimits(	const double value, const size_t idx);
	void setLower(	const double lower)			{ setLimits(lower,	0);	}
	void setUpper(	const double upper)			{ setLimits(upper,	1);	}

signals:
	void titleChanged(		QString title);
	void titleTypeChanged(	TitleType title);
	void typeChanged(		QString type);
	void verticalChanged(	bool	transposed);
	void rangeChanged();
	void limitsChanged();
	void continuousChanged();
	void somethingChanged();
	void hasBreaksChanged();
	void addToUndoStack();

private:
	std::string				TitleTypeToString (TitleType  type) const;
	std::string				BreaksTypeToString(BreaksType type) const;
	std::string				LimitsTypeToString(LimitsType type) const;

	TitleType				TitleTypeFromString (const std::string& type) const;
	BreaksType				BreaksTypeFromString(const std::string& type) const;
	LimitsType				LimitsTypeFromString(const std::string& type) const;

private:
	PlotEditorModel		*	_plotEditor = nullptr;
	QString					_title,
							_type,
							_axisType;
	TitleType				_titleType;
	BreaksType				_breaksType;
	LimitsType				_limitsType;
	std::vector<double>		_range,
							_breaks,
							_limits;
	std::vector<QString>	_labels;
	bool					_vertical	= false,
							_continuous	= false; // <- required to avoid warning about "member not initialized"
	Json::Value				_axis		= Json::objectValue;

	//add expands as well?

	void					fillFromJSON(std::vector<double>	&obj,	Json::Value value);
	void					fillFromJSON(std::vector<QString>	&obj,	Json::Value value);
};

}

Q_DECLARE_METATYPE(PlotEditor::AxisModel::TitleType);
Q_DECLARE_METATYPE(PlotEditor::AxisModel::BreaksType);
Q_DECLARE_METATYPE(PlotEditor::AxisModel::LimitsType);

#endif // PLOTEDITORAXISMODEL_H
