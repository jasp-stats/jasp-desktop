#ifndef HELPMODEL_H
#define HELPMODEL_H

#include <QObject>
#include "languagemodel.h"

class HelpModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged	)
	Q_PROPERTY(QString	pagePath	READ pagePath	WRITE setPagePath	NOTIFY pagePathChanged	)

public:
			HelpModel(QObject * parent);

	bool	visible()	const { return _visible;  }
	QString pagePath()	const { return _pagePath; }

public slots:
	void	setVisible(bool visible);
	void	setPagePath(QString pagePath);
	void	setAnalysispagePath(QString analysisName) { setPagePath("analyses/" + analysisName); }
	void	generateJavascript();
	void	showOrTogglePage(QString pagePath);
	QString	indexURL();
	void	reloadPage();
	void	setThemeCss(QString themeName);

signals:
	void renderCode(QString javascript);
	void visibleChanged(bool visible);
	void pagePathChanged(QString pagePath);
	void runJavaScript(QString helpJS);

private:
	QString convertPagePathToLower(const QString & pagePath);

private:
	bool	_visible	= false;
	QString _pagePath	= "";
};

#endif // HELPMODEL_H
