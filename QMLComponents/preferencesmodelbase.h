#ifndef PREFERENCESMODELBASE_H
#define PREFERENCESMODELBASE_H

#include <QObject>

class PreferencesModelBase : public QObject
{
	Q_OBJECT
public:
	explicit PreferencesModelBase(QObject *parent = nullptr);
	~PreferencesModelBase() { _singleton = nullptr; }

	virtual double	uiScale()						{ return 1;	}
	virtual int		maxFlickVelocity()		const	{ return 808; }

	static PreferencesModelBase* prefs();

public slots:
	void			currentThemeNameHandler();
	virtual void	setCurrentThemeName(QString currentThemeName) {}

signals:
	void uiScaleChanged();
	void maxFlickVelocityChanged();
	void currentJaspThemeChanged();
	void currentThemeReady();
	void interfaceFontChanged();


protected:
	static PreferencesModelBase* _singleton;

};

#endif // PREFERENCESMODELBASE_H
