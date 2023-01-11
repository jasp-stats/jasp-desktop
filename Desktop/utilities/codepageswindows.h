#ifndef CODEPAGESWINDOWS_H
#define CODEPAGESWINDOWS_H

#include <QObject>
#include <QString>
#include <QTimer>
#include <string>

class CodePagesWindows : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QStringList	codePageIDs		READ codePageIDs									NOTIFY codePageIDsChanged		)
	Q_PROPERTY(QString		codePageID		READ codePageID			WRITE setCodePageID			NOTIFY codePageIDChanged		)
	Q_PROPERTY(int			codePage		READ codePage										NOTIFY codePageChanged			)
	Q_PROPERTY(bool			error			READ error				WRITE setError				NOTIFY errorChanged				)

public:
	explicit							CodePagesWindows(QObject *parent = nullptr);

	static	std::string					convertCodePageStrToUtf8(	const std::string	& raw					);
	static	void						addCodepageIdentifier(		const std::wstring	& codePageIdentifier	);
			void						startTimer();
	
			const QStringList		&	codePageIDs()		const;
			const QString			&	codePageID()		const;
			int							codePage()			const;
		
			void						setCodePageID(			const QString		& newCurrentCodePage);
			void						setCodePage(			int newCodePage);
			void						determineCodePageNumber();
	
signals:
			void						codePageIDsChanged();
			void						codePageIDChanged();
			void						codePageChanged();
	
private:
	static	CodePagesWindows		*	_singleton;
	
			QStringList					_codePageIDs;
			QString						_codePageID;
			int							_codePage;
			QTimer						_timer;
			
};

#endif // CODEPAGESWINDOWS_H
