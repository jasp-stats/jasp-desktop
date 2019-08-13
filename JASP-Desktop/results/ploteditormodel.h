#ifndef PLOTEDITORMODEL_H
#define PLOTEDITORMODEL_H

#include <QObject>
#include "jsonredirect.h"
#include "ploteditoraxismodel.h"

class Analyses;
class Analysis;

class PlotEditorModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool						visible			READ visible		WRITE setVisible		NOTIFY visibleChanged		)
	Q_PROPERTY(QString					name			READ name			WRITE setName			NOTIFY nameChanged			)
	Q_PROPERTY(QString					data			READ data			WRITE setData			NOTIFY dataChanged			)
	Q_PROPERTY(QString					title			READ title			WRITE setTitle			NOTIFY titleChanged			)
	Q_PROPERTY(int						width			READ width			WRITE setWidth			NOTIFY widthChanged			)
	Q_PROPERTY(int						height			READ height			WRITE setHeight			NOTIFY heightChanged		)
	Q_PROPERTY(PlotEditorAxisModel *	xAxis			READ xAxis									NOTIFY dummyAxisChanged		)
	Q_PROPERTY(PlotEditorAxisModel *	yAxis			READ yAxis									NOTIFY dummyAxisChanged		)


public:
	explicit PlotEditorModel(Analyses * analyses);

	bool					visible()	const {	return _visible;	}
	QString					name()		const { return _name;		}
	QString					data()		const;
	QString					title()		const { return _title;		}
	int						width()		const { return _width;		}
	int						height()	const { return _height;		}
	PlotEditorAxisModel *	xAxis()		const { return _xAxis;		}
	PlotEditorAxisModel *	yAxis()		const { return _yAxis;		}

signals:
	void visibleChanged(		bool		visible			);
	void nameChanged(			QString		name			);
	void dataChanged(			QString		data			);
	void titleChanged(			QString		title			);
	void widthChanged(			int			width			);
	void heightChanged(			int			height			);
	void dummyAxisChanged();

public slots:
	void showPlotEditor(int id, QString options);

	void setVisible(		bool		visible			);
	void setName(			QString		name			);
	void setData(			QString		data			);
	void setTitle(			QString		title			);
	void setWidth(			int			width			);
	void setHeight(			int			height			);

	void somethingChanged() const;


private:
	Analyses			*	_analyses		= nullptr;
	Analysis			*	_analysis		= nullptr;
	PlotEditorAxisModel *	_xAxis			= nullptr,
						*	_yAxis			= nullptr;

	Json::Value				_editOptions	= Json::nullValue,
							_imgOptions		= Json::nullValue;
	QString					_name,
							_data,
							_title;
	bool					_visible		= false;
	int						_width,
							_height,
							_analysisId;
};

#endif // PLOTEDITORMODEL_H
