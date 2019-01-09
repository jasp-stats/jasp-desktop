#ifndef HELPMODEL_H
#define HELPMODEL_H

#include <QObject>

class HelpModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged	)
	Q_PROPERTY(QString	pageName	READ pageName	WRITE setPageName	NOTIFY pageNameChanged	)
	Q_PROPERTY(QString	helpJS		READ helpJS							NOTIFY helpJSChanged	)

public:
			HelpModel(QObject * parent) : QObject(parent)
			{
				setPageName("index");
				connect(this, &HelpModel::pageNameChanged, this, &HelpModel::generateJavascript);
			}

	bool	visible()	const { return _visible;  }
	QString pageName()	const { return _pageName; }
	QString helpJS()	const { return _helpJS;  }

public slots:
	void	setVisible(bool visible);
	void	setPageName(QString pageName);
	void	setAnalysisPagename(QString analysisName) { setPageName("analyses/" + analysisName); }
	void	generateJavascript();
	void	setHelpJS(QString helpJS);
	void	showOrTogglePage(QString pageName);
	QString	indexURL();


signals:
	void renderCode(QString javascript);
	void visibleChanged(bool visible);
	void pageNameChanged(QString pageName);
	void helpJSChanged(QString helpJS);

private:
	bool	_visible	= false;
	QString _pageName	= "",
			_helpJS		= "";
};

#endif // HELPMODEL_H
