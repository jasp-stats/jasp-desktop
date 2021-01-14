#ifndef HELPMODEL_H
#define HELPMODEL_H

#include <QObject>
#include "languagemodel.h"

class HelpModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged	)
	Q_PROPERTY(QString	pagePath	READ pagePath	WRITE setPagePath	NOTIFY pagePathChanged	)
	Q_PROPERTY(QString	markdown	READ markdown	WRITE setMarkdown	NOTIFY markdownChanged	)

public:
			HelpModel(QObject * parent);

	bool	visible()	const { return _visible;  }
	QString pagePath()	const { return _pagePath; }
	QString markdown()	const { return _markdown; }
	void	runJavaScript(QString renderFunc, QString content);

public slots:
	void	setVisible(bool visible);
	void	setPagePath(QString pagePath);
	void	setAnalysispagePath(QString analysisName) { setPagePath("analyses/" + analysisName); }
	void	generateJavascript();
	void	showOrTogglePage(QString pagePath);
	QString	indexURL();
	void	reloadPage();
	void	setThemeCss(QString themeName);
	void	setFont();
	void	loadingSucceeded();
	void	setMarkdown(QString markdown);
	void	loadMarkdown(QString md);
	bool	pageExists(QString pagePath);

signals:
	void renderCode(QString javascript);
	void visibleChanged(bool visible);
	void pagePathChanged(QString pagePath);
	void runJavaScriptSignal(QString helpJS);

	void markdownChanged(QString markdown);

private:
	QString convertPagePathToLower(const QString & pagePath);
	bool loadHelpContent(const QString & pagePath, bool ignorelanguage, QString &renderFunc, QString &content);

private:
	bool	_visible	= false;
	QString _pagePath	= "",
			_markdown	= "";
};

#endif // HELPMODEL_H
