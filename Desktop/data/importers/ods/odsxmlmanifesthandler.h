#ifndef ODSXMLMANIFESTHANDLER_H
#define ODSXMLMANIFESTHANDLER_H


#include "odsxmlhandler.h"
#include "odsimportdataset.h"

namespace ods
{

class XmlManifestHandler : public XmlHandler
{
public:
	XmlManifestHandler(ODSImportDataSet *data);


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
	virtual bool startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &atts);

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
	virtual bool endElement(const QString &namespaceURI, const QString &localName, const QString &qName);

	/**
	 * @brief characters Called when char data found.
	 * @param ch The found data.
	 * @return true on no error.
	 */
	virtual bool characters(const QString &ch);

private:
	bool	_foundRoot;	/**< Found archive root in manifest? */
};

} // end namespace

#endif // ODSXMLMANIFESTHANDLER_H
