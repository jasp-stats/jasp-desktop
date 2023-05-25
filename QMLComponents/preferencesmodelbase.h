#ifndef PREFERENCESMODELBASE_H
#define PREFERENCESMODELBASE_H

#include <QObject>

class PreferencesModelBase : public QObject
{
	Q_OBJECT
public:
	explicit PreferencesModelBase(QObject *parent = nullptr);
	~PreferencesModelBase() { _singleton = nullptr; }

	virtual double	uiScale()						{ return 1;		}
	virtual int		maxFlickVelocity()		const	{ return 808;	}
	virtual bool	showRSyntax()			const	{ return false; }
	virtual bool	showAllROptions()		const	{ return false; }

	static PreferencesModelBase* preferences();

public slots:
	void			currentThemeNameHandler();
	virtual void	setCurrentThemeName(QString currentThemeName)	{}
	virtual void	setShowRSyntax(bool showRSyntax)				{}
	virtual void	setShowAllROptions(bool showAllROptions)		{}

signals:
	void uiScaleChanged();
	void maxFlickVelocityChanged();
	void currentJaspThemeChanged();
	void currentThemeReady();
	void interfaceFontChanged();
	void showRSyntaxChanged();
	void showAllROptionsChanged();

protected:
	static PreferencesModelBase* _singleton;

};

#endif // PREFERENCESMODELBASE_H
