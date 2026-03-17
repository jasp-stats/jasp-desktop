#ifndef ENCRYPTIONSETTINGSMODEL_H
#define ENCRYPTIONSETTINGSMODEL_H

#include <QObject>

class EncryptionSettingsModel : public QObject
{
	Q_OBJECT
    Q_PROPERTY(bool		 visible            READ visible                 WRITE setVisible                NOTIFY visibleChanged				)
    Q_PROPERTY(QString	 password           READ password				WRITE setPassword               NOTIFY passwordChanged				)
    Q_PROPERTY(bool		 jaspSubmission     READ jaspSubmission			WRITE setJaspSubmission         NOTIFY jaspSubmissionChanged		)
    Q_PROPERTY(bool		 encryptionActive   READ encryptionActive		WRITE setEncryptionActive       NOTIFY encryptionActiveChanged		)
    Q_PROPERTY(QString	 publickey          READ publickey				WRITE setPublickey              NOTIFY publickeyChanged				)
    Q_PROPERTY(QString	 privatekey         READ privatekey  			WRITE setPrivatekey             NOTIFY privatekeyChanged				)

public:
	explicit EncryptionSettingsModel(QObject *parent = nullptr);

	QString password() const;
	void setPassword(const QString &newPassword);

	bool jaspSubmission() const;
	void setJaspSubmission(bool value);

	bool encryptionActive() const;
	void setEncryptionActive(bool newEncryptionActive);

	bool visible() const;
	void setVisible(bool newVisible);

	Q_INVOKABLE void submit();
	Q_INVOKABLE void cancel();

    QString publickey() const;
    void setPublickey(const QString &newPublickey);

    QString privatekey() const;
    void setPrivatekey(const QString &newPrivatekey);

signals:
	void queryComplete();
	void passwordChanged();
	void jaspSubmissionChanged();
	void encryptionActiveChanged();
	void visibleChanged();
    void publickeyChanged();
    void privatekeyChanged();

public slots:
	void queryEncryptionSettings();

private:
	bool _visible = false;
};

#endif // ENCRYPTIONSETTINGSMODEL_H
