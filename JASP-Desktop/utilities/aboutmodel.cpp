#include "aboutmodel.h"
#include "appinfo.h"

AboutModel::AboutModel(QObject *parent) : QObject(parent)
{
	QString copyrightmessage = getJaspCopyrightMessage();
	setJaspCopyrightMessage(copyrightmessage);

	QString buildString = QString::fromStdString(AppInfo::builddate) ;  AppInfo::builddate;
	setJaspBuildDate(buildString);

	QString versionString = getJaspVersion();
	setJaspVersionString("Version " + versionString);

	QString downloadurl = getJaspDownloadUrl();
	setJaspDownloadUrl(downloadurl);

	QString citation = getJaspCitation();
	setJaspCitation(citation);

	QString warranty = getJaspWarranty();
	setJaspWarranty(warranty);

	QString citationUrl = getJaspCitationUrl();
	setJaspCitationUrl(citationUrl);


}

bool AboutModel::visible() const
{
	return m_visible;
}

QString AboutModel::jaspVersionString() const
{
	return m_jaspVersionString;
}

QString AboutModel::jaspBuildDate() const
{
	return m_jaspBuildDate;
}

QString AboutModel::getJaspVersion()
{
	QString version = QString::fromStdString(AppInfo::version.asString());

#ifdef JASP_DEBUG
	version+="-Debug";
#endif

	version+="-Beta";

	return version;
}

QString AboutModel::getJaspCopyrightMessage()
{
	QString copyrightmessage;

	copyrightmessage = "Copyright 2013-" + QString::fromStdString(AppInfo::getBuildYear()) + " University of Amsterdam";

	return copyrightmessage;
}

QString AboutModel::getJaspDownloadUrl()
{
	QString downloadurl = "https://jasp-stats.org/download/";

	return downloadurl;
}

QString AboutModel::getJaspCitation()
{

	QString citation = "JASP Team (" +  QString::fromStdString(AppInfo::getBuildYear()) + "). JASP (" + getJaspVersion() +")[Compute software].";

	return citation;
}

QString AboutModel::getJaspWarranty()
{

	QString warranty = "This program is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING <br>THE WARRANTY OF DESIGN, ";
			warranty+= "MERCHANTABILITY AND FITNESS FOR A <br> PARTICULAR PURPOSE.";

	return warranty;

}

QString AboutModel::getJaspCitationUrl()
{
	QString citationUrl = "https://jasp-stats.org/faq/how-do-i-cite-jasp/";

	return citationUrl;
}

QString AboutModel::jaspCitationUrl() const
{
	return m_jaspCitationUrl;
}

QString AboutModel::jaspCopyrightMessage() const
{
	return m_jaspCopyrightMessage;
}

QString AboutModel::jaspDownloadUrl() const
{
	return m_jaspDownloadURL;
}

QString AboutModel::jaspCitation() const
{
	return m_jaspCitation;
}

QString AboutModel::jaspWarranty() const
{
	return m_jaspWarranty;
}

void AboutModel::setVisible(bool visible)
{
	if (m_visible == visible)
		return;

	m_visible = visible;
	emit visibleChanged(m_visible);
}

void AboutModel::setJaspVersionString(QString jaspVersionString)
{
	if (m_jaspVersionString == jaspVersionString)
		return;

	m_jaspVersionString = jaspVersionString;
	emit jaspVersionStringChanged(m_jaspVersionString);
}

void AboutModel::setJaspBuildDate(QString jaspBuildDate)
{
	if (m_jaspBuildDate == jaspBuildDate)
		return;

	m_jaspBuildDate = jaspBuildDate;
	emit jaspBuildDateChanged(m_jaspBuildDate);
}

void AboutModel::setJaspCopyrightMessage(QString copyrightMessage)
{
	m_jaspCopyrightMessage = copyrightMessage;
}

void AboutModel::setJaspDownloadUrl(QString downloadURL)
{
	m_jaspDownloadURL = downloadURL;
}

void AboutModel::setJaspCitation(QString citation)
{
	m_jaspCitation = citation;
}

void AboutModel::setJaspWarranty(QString warranty)
{
	m_jaspWarranty = warranty;
}

void AboutModel::setJaspCitationUrl(QString jaspCitationUrl)
{
	if (m_jaspCitationUrl == jaspCitationUrl)
		return;

	m_jaspCitationUrl = jaspCitationUrl;
	emit jaspCitationUrlChanged(m_jaspCitationUrl);
}
