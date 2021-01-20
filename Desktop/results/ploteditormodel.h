#ifndef PLOTEDITORMODEL_H
#define PLOTEDITORMODEL_H

#include <QUrl>
#include <QObject>
#include "jsonredirect.h"
#include "ploteditoraxismodel.h"
#include "ploteditorcoordinates.h"
#include <stack>

class Analyses;
class Analysis;

namespace PlotEditor
{

class PlotEditorModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool						visible			READ visible		WRITE setVisible		NOTIFY visibleChanged		)
	Q_PROPERTY(QString					name			READ name			WRITE setName			NOTIFY nameChanged			)
	Q_PROPERTY(QString					data			READ data			WRITE setData			NOTIFY dataChanged			)
	Q_PROPERTY(QUrl						imgFile			READ imgFile								NOTIFY dataChanged			)
	Q_PROPERTY(QString					title			READ title			WRITE setTitle			NOTIFY titleChanged			)
	Q_PROPERTY(int						width			READ width			WRITE setWidth			NOTIFY widthChanged			)
	Q_PROPERTY(int						height			READ height			WRITE setHeight			NOTIFY heightChanged		)
	Q_PROPERTY(AxisModel *				xAxis			READ xAxis									NOTIFY dummyAxisChanged		)
	Q_PROPERTY(AxisModel *				yAxis			READ yAxis									NOTIFY dummyAxisChanged		)
	Q_PROPERTY(double					ppi				READ ppi									NOTIFY ppiChanged			)
	Q_PROPERTY(bool						loading			READ loading		WRITE setLoading		NOTIFY loadingChanged		)
	Q_PROPERTY(bool						advanced		READ advanced		WRITE setAdvanced		NOTIFY advancedChanged		)
	Q_PROPERTY(bool						undoEnabled		READ undoEnabled	WRITE setUndoEnabled	NOTIFY undoEnabledChanged	)
	Q_PROPERTY(bool						redoEnabled		READ redoEnabled	WRITE setRedoEnabled	NOTIFY redoEnabledChanged	)


public:
	explicit PlotEditorModel();

	bool					visible()	const {	return _visible;	}
	QString					name()		const { return _name;		}
	QString					data()		const { return _data;		}
	QUrl					imgFile()	const;
	QString					title()		const { return _title;		}
	int						width()		const { return _width;		}
	int						height()	const { return _height;		}
	AxisModel			*	xAxis()		const { return _xAxis;		}
	AxisModel			*	yAxis()		const { return _yAxis;		}
	double					ppi()		const {	return _ppi;		}
	bool					loading()	const { return _loading;	}
	bool					advanced()	const {	return _advanced;	}
	void					reset();

	bool					undoEnabled()	const {	return _undoEnabled;	}
	bool					redoEnabled()	const {	return _redoEnabled;	}

signals:
	void visibleChanged(		bool		visible			);
	void nameChanged(			QString		name			);
	void dataChanged(										);
	void titleChanged(			QString		title			);
	void widthChanged(			int			width			);
	void heightChanged(			int			height			);
	void dummyAxisChanged();
	void ppiChanged();// TODO, refresh all
	void resetPlotChanged(		bool		resetPlot		);
	void loadingChanged(		bool		loading			);
	void advancedChanged(		bool		advanced		);
	void undoEnabledChanged(	bool		undoEnabled		);
	void redoEnabledChanged(	bool		redoEnabled		);

	void saveImage(int	id,	QString		options)	const;


public slots:
	void showPlotEditor(int id, QString options);

	void setVisible(		bool					visible			);
	void setName(			const QString	&		name			);
	void setData(			const QString	&		data			);
	void setTitle(			const QString	&		title			);
	void setWidth(			int						width			);
	void setHeight(			int						height			);
	void setLoading(		bool					loading			);
	void setAdvanced(		bool					advanced		);
	
	void					resetPlot();
	void					savePlot()	const;

	void somethingChanged();
	void refresh();

	QString clickHitsElement(double x, double y) const;
	
	void setUndoEnabled(bool undoEnabled);
	void setRedoEnabled(bool redoEnabled);

	Q_INVOKABLE	void undoSomething();
	Q_INVOKABLE	void redoSomething();
	void applyChangesFromUndoOrRedo();


private:
	void		processImgOptions();
	Json::Value generateImgOptions()	const;
	Json::Value generateEditOptions()	const;

	void		clear(std::stack<Json::Value>& x);

private:
	Analysis			*	_analysis		= nullptr;
	AxisModel			*	_xAxis			= nullptr,
						*	_yAxis			= nullptr;

	Coordinates				_coordinates;
	Json::Value				_editOptions	= Json::nullValue,
							_imgOptions		= Json::nullValue,
							_prevImgOptions	= Json::nullValue,
							_originalImgOps	= Json::nullValue;
	QString					_name,
							_data,
							_title,
							_options;
	bool					_visible		= false,
							_goBlank		= false,
							_loading		= false,
							_advanced		= false,
							_redoEnabled	= false,
							_undoEnabled	= false;
	int						_width,
							_height,
							_analysisId,
							_lastAnalID;
	double					_ppi;

	static int				_editRequest;

	std::stack<Json::Value>	_undo,
							_redo;

};

}

#endif // PLOTEDITORMODEL_H

