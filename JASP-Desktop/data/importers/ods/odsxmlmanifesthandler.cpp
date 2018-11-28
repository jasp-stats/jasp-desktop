#include "odsxmlmanifesthandler.h"


using namespace std;
using namespace ods;

XmlManifestHandler::XmlManifestHandler(ODSImportDataSet *data)
 : XmlHandler(data)
 , _foundRoot(false)
{

}

/**
 * @brief startElement Called on the start of an element.
 * @param namespaceURI - the URI.
 * @param localName - local name (name without prefix).
 * @param qName - Qualified name.
 * @param atts- Attributes.
 * @return true on no error found.
 *
 * Called when a <tag ...> construction found.
 *
 */
bool XmlManifestHandler::startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &atts)
{
	static const QString localNameFileEntry("file-entry");
	static const QString attNamemediaType("manifest:media-type");
	static const QString attNameFullPath("manifest:full-path");
	static const QString sheetMediaType("application/vnd.oasis.opendocument.spreadsheet");
	static const QString root("/");

	if (localName == localNameFileEntry)
	{
		QString fullPath = atts.value(attNameFullPath);
		QString mediaType = atts.value(attNamemediaType);
		QRegExp rx(_dataSet->contentRegExpression, Qt::CaseInsensitive);

		// are we a spread-sheet?
		if ((fullPath == root) && (!_foundRoot))
		{
			_foundRoot = true;
			if (mediaType != sheetMediaType)
				throw runtime_error("File is not a ODS spreadsheet.");

		}
		// Got contents file?
		else if ((rx.indexIn(fullPath) != -1) && (_foundRoot))
		{ // Found a content file name.
			_dataSet->setContentFilename(fullPath.toStdString());
		}
	}

	return true;
}

/**
 * @brief endElement Called on the end of an element.
 * @param namespaceURI - the URI.
 * @param localName - local name (name without prefix).
 * @param qName - Qualified name.
 * @param atts- Attributes.
 * @return true on no error found.
 *
 * Called when a </tag> construction found.
 *
 */
bool XmlManifestHandler::endElement(const QString &namespaceURI, const QString &localName, const QString &qName)
{

	return true;
}

/**
 * @brief characters Called when char data found.
 * @param ch The found data.
 * @return true on no error.
 */
bool XmlManifestHandler::characters(const QString &ch)
{

	return true;
}
