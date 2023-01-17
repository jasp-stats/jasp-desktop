#ifndef CODEPAGESWINDOWS_H
#define CODEPAGESWINDOWS_H

#include <QObject>
#include <QString>
#include <QTimer>
#include <QMap>
#include <string>

class CodePagesWindows : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QStringList	codePageIDs		READ codePageIDs									NOTIFY codePageIDsChanged		)
	Q_PROPERTY(QString		codePageID		READ codePageID			WRITE setCodePageID			NOTIFY codePageIDChanged		)
	Q_PROPERTY(bool			error			READ error				WRITE setError				NOTIFY errorChanged				)

public:
	explicit							CodePagesWindows(QObject *parent = nullptr);

	static	std::string					convertCodePageStrToUtf8(	const std::string	& raw					);
	static	void						addCodepageIdentifier(		const std::wstring	& codePageIdentifier	);
	
			const QStringList		&	codePageIDs()		const;
			const QString			&	codePageID()		const;
			bool						error()				const;
		
			void						setCodePageID(			const QString		&	newCurrentCodePage	);
			void						setError(				bool					newError			);

signals:
			void						codePageIDsChanged();
			void						codePageIDChanged();
			void                        errorChanged();

private:
	static	CodePagesWindows		*	_singleton;
	
			QStringList					_codePageIDs;		///< Human readable descriptions of the codepages
			QString						_codePageID;		///< Currently selected human-readable codePageID
			QMap<QString, int>			_codePageIDMap;		///< Maps human-readable to codepage number
			QTimer						_timer;
			bool                        _error	= false;	///< Something went wrong if this is true...
};

#endif // CODEPAGESWINDOWS_H
