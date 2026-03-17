#include "encryptionsettingsmodel.h"
#include "data/jaspencryptiondata.h"
#include "utilities/qutils.h"


EncryptionSettingsModel::EncryptionSettingsModel(
	QObject *parent)
	: QObject{parent}
{}

QString EncryptionSettingsModel::password() const
{
	return tq(JaspEncryptionData::getInstance()->getPassword());
}

void EncryptionSettingsModel::setPassword(const QString &newPassword)
{
	if (password() == newPassword)
		return;
	JaspEncryptionData::getInstance()->setPassword(fq(newPassword));
	emit passwordChanged();
}

bool EncryptionSettingsModel::jaspSubmission() const
{
	return JaspEncryptionData::getInstance()->jaspTeamSubmission();
}

void EncryptionSettingsModel::setJaspSubmission(bool value)
{
	if (jaspSubmission() == value)
		return;
	JaspEncryptionData::getInstance()->setJaspTeamSubmission(value);
	emit jaspSubmissionChanged();
}

bool EncryptionSettingsModel::encryptionActive() const
{
	return JaspEncryptionData::getInstance()->encryptionActive();
}

void EncryptionSettingsModel::setEncryptionActive(bool value)
{
	if (encryptionActive() == value)
		return;
	JaspEncryptionData::getInstance()->setEncryptionActive(value);
	emit encryptionActiveChanged();
}

void EncryptionSettingsModel::queryEncryptionSettings()
{
	JaspEncryptionData::getInstance()->reset();
	
    emit passwordChanged();
    emit jaspSubmissionChanged();
    emit encryptionActiveChanged();
    emit publickeyChanged();
    emit privatekeyChanged();
	setVisible(true);
}

bool EncryptionSettingsModel::visible() const
{
	return _visible;
}

void EncryptionSettingsModel::setVisible(bool newVisible)
{
	if (_visible == newVisible)
		return;
	_visible = newVisible;
	emit visibleChanged();
}

void EncryptionSettingsModel::submit()
{
	setVisible(false);
	
	JaspEncryptionData::getInstance()->setParamsSet(true);
	emit queryComplete();
}

void EncryptionSettingsModel::cancel()
{
	setVisible(false);
		
	emit queryComplete();
}

QString EncryptionSettingsModel::publickey() const
{
    return tq(JaspEncryptionData::getInstance()->getPublicKeyResponse());
}

void EncryptionSettingsModel::setPublickey(const QString &newPublickey)
{
    if (newPublickey == publickey())
        return;
    JaspEncryptionData::getInstance()->setPublicKeyResponse(fq(newPublickey));
    emit publickeyChanged();
}

QString EncryptionSettingsModel::privatekey() const
{
    return tq(JaspEncryptionData::getInstance()->getPrivatekey());
}

void EncryptionSettingsModel::setPrivatekey(const QString &newPrivatekey)
{
    if (newPrivatekey == privatekey())
        return;
    JaspEncryptionData::getInstance()->setPrivatekey(fq(newPrivatekey));
    emit publickeyChanged();
}
