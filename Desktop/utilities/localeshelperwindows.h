#ifndef LOCALESHELPERWINDOWS_H
#define LOCALESHELPERWINDOWS_H

#include <QObject>
#include <QString>
#include <string>

class LocalesHelperWindows : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QStringList	locales			READ locales		WRITE setLocales		NOTIFY localesChanged		)
	Q_PROPERTY(QString		currentLocale	READ currentLocale	WRITE setCurrentLocale	NOTIFY currentLocaleChanged	)
	Q_PROPERTY(int			codePage		READ codePage		WRITE setCodePage		NOTIFY codePageChanged		)

public:
	explicit LocalesHelperWindows(QObject *parent = nullptr);

	static	std::string				convertCodepagedStrToUtf8(const std::string & raw);
	
			const QStringList	&	locales()			const;
			const QString		&	currentLocale()		const;
			int						codePage()			const;
		
			void					setLocales(			const QStringList &newLocales);
			void					setCurrentLocale(	const QString &newCurrentLocale);
			void					setCodePage(		int newCodePage);
	
signals:
	void localesChanged();
	void currentLocaleChanged();
	void codePageChanged();
	
private:
	static	LocalesHelperWindows	*	_singleton;
	
			QStringList					_locales;
			QString						_currentLocale;
			int							_codePage;
};

#endif // LOCALESHELPERWINDOWS_H
