#ifndef ABOUTMODEL_H
#define ABOUTMODEL_H

#include <QObject>

class AboutModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY(bool visible					READ visible				WRITE setVisible				NOTIFY visibleChanged)
	Q_PROPERTY(QString jaspVersionString	READ jaspVersionString		WRITE setJaspVersionString		NOTIFY jaspVersionStringChanged)
	Q_PROPERTY(QString jaspBuildDate		READ jaspBuildDate			WRITE setJaspBuildDate			NOTIFY jaspBuildDateChanged)
	Q_PROPERTY(QString jaspCopyrightMessage	READ jaspCopyrightMessage	WRITE setJaspCopyrightMessage	NOTIFY jaspCopyrightMessageChanged)
	Q_PROPERTY(QString jaspDownloadUrl		READ jaspDownloadUrl		WRITE setJaspDownloadUrl		NOTIFY aspDownloadURLChanged)
	Q_PROPERTY(QString jaspCitation			READ jaspCitation			WRITE setJaspCitation			NOTIFY jaspCitationChanged)
	Q_PROPERTY(QString jaspWarranty			READ jaspWarranty			WRITE setJaspWarranty			NOTIFY jaspWarrantyChanged)
	Q_PROPERTY(QString jaspCitationUrl		READ jaspCitationUrl		WRITE setJaspCitationUrl		NOTIFY jaspCitationUrlChanged)

public:
	explicit AboutModel(QObject *parent = nullptr);
	bool visible() const;
	QString jaspVersionString() const;
	QString jaspBuildDate() const;
	QString jaspCopyrightMessage() const;
	QString jaspDownloadUrl() const;
	QString jaspCitation() const;
	QString jaspWarranty() const;
	QString jaspCitationUrl() const;

	static QString getJaspVersion();
	static QString getJaspCopyrightMessage();
	static QString getJaspDownloadUrl();
	static QString getJaspCitation();
	static QString getJaspWarranty();
	static QString getJaspCitationUrl();

signals:
	void visibleChanged(bool visible);
	void jaspVersionStringChanged(QString jaspVersionString);
	void jaspBuildDateChanged(QString jaspBuildDate);	
	void jaspWarrantyChanged(QString jaspWarranty);
	void jaspCitationChanged(QString jaspCitation);
	void aspDownloadURLChanged(QString jaspDownloadUrl);
	void jaspCopyrightMessageChanged(QString jaspCopyrightMessage);	
	void jaspCitationUrlChanged(QString jaspCitationUrl);

public slots:
	void setVisible(bool visible);
	void setJaspVersionString(QString jaspVersionString);
	void setJaspBuildDate(QString jaspBuildDate);
	void setJaspCopyrightMessage(QString jaspCopyrightMessage);
	void setJaspDownloadUrl(QString downloadURL);
	void setJaspCitation(QString citation);
	void setJaspWarranty(QString warrenty);

	void setJaspCitationUrl(QString jaspCitationUrl);

private:
	bool m_visible = false;
	QString m_jaspVersionString = "";
	QString m_jaspBuildDate = "";
	QString m_jaspCopyrightMessage;
	QString m_jaspDownloadURL;
	QString m_jaspCitation;
	QString m_jaspWarranty;
	QString m_jaspCitationUrl;
};

#endif // ABOUTMODEL_H
