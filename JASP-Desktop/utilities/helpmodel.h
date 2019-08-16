#ifndef HELPMODEL_H
#define HELPMODEL_H

#include <QObject>
#include "languagemodel.h"

class HelpModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged	)
	Q_PROPERTY(QString	pagePath	READ pagePath	WRITE setPagePath	NOTIFY pagePathChanged	)
	Q_PROPERTY(QString	helpJS		READ helpJS							NOTIFY helpJSChanged	)

public:
			HelpModel(QObject * parent) : QObject(parent)
			{
				setPagePath("index");
				connect(this, &HelpModel::pagePathChanged, this, &HelpModel::generateJavascript);
			}

	bool	visible()	const { return _visible;  }
	QString pagePath()	const { return _pagePath; }
	QString helpJS()	const { return _helpJS;  }

public slots:
	void	setVisible(bool visible);
	void	setPagePath(QString pagePath);
	void	setAnalysispagePath(QString analysisName) { setPagePath("analyses/" + analysisName); }
	void	generateJavascript();
	void	setHelpJS(QString helpJS);
	void	showOrTogglePage(QString pagePath);
	QString	indexURL();
	void	reloadPage();


signals:
	void renderCode(QString javascript);
	void visibleChanged(bool visible);
	void pagePathChanged(QString pagePath);
	void helpJSChanged(QString helpJS);

private:
	QString convertPagePathToLower(const QString & pagePath);

private:
	bool	_visible	= false;
	QString _pagePath	= "",
			_helpJS		= "";
};

#endif // HELPMODEL_H
