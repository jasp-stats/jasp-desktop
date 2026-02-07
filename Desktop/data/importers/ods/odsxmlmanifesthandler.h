#ifndef ODSXMLMANIFESTHANDLER_H
#define ODSXMLMANIFESTHANDLER_H

#include <QXmlStreamReader>
#include "odsxmlhandler.h"
#include "odsimportdataset.h"

namespace ods
{

class XmlManifestHandler : public XmlHandler
{
public:
	explicit XmlManifestHandler(ods::ODSImportDataSet *data);

	bool parse(QXmlStreamReader &reader);

private:
	bool _foundRoot = false; /**< Found archive root in manifest? */
};

} // namespace ods

#endif // ODSXMLMANIFESTHANDLER_H
