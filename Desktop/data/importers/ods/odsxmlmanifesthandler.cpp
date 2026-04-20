#include <QRegularExpression>
#include "odsxmlmanifesthandler.h"

namespace ods
{

XmlManifestHandler::XmlManifestHandler(ods::ODSImportDataSet *data)
	: XmlHandler(data)
	, _foundRoot(false)
{
}

bool XmlManifestHandler::parse(QXmlStreamReader &reader)
{
	static const QString localNameFileEntry("file-entry");
	static const QString attNameFullPath("manifest:full-path");
	static const QString attNamemediaType("manifest:media-type");
	static const QString sheetMediaType("application/vnd.oasis.opendocument.spreadsheet");
	static const QString rootPath("/");

	const QRegularExpression rx(_dataSet->contentRegExpression, QRegularExpression::CaseInsensitiveOption);

	while (!reader.atEnd() && !reader.hasError())
	{
		QXmlStreamReader::TokenType token = reader.readNext();

		if (token == QXmlStreamReader::StartElement)
		{
			if (reader.name() == localNameFileEntry)
			{
				QXmlStreamAttributes atts = reader.attributes();
				QString fullPath  = atts.value(attNameFullPath).toString();
				QString mediaType = atts.value(attNamemediaType).toString();

				if (fullPath == rootPath && !_foundRoot)
				{
					_foundRoot = true;
					if (mediaType != sheetMediaType)
							throw std::runtime_error("File is not a ODS spreadsheet.");
				}
				else if (_foundRoot && rx.match(fullPath).hasMatch())
				{
					_dataSet->setContentFilename(fullPath.toStdString());
				}
			}
		}
	}

	return !reader.hasError();
}

} // namespace ods
