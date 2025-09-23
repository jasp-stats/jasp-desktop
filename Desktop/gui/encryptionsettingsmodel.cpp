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
	JaspEncryptionData::getInstance()->setParamsSet(true);
	emit queryComplete();
}
