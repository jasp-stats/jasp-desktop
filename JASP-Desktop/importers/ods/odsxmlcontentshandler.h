#ifndef ODSXMLCONTENTSHANDLER_H
#define ODSXMLCONTENTSHANDLER_H

#include <vector>

#include "odsxmlhandler.h"

namespace ods
{

class XmlContentsHandler : public XmlHandler
{
	// Depth in XML document.
	typedef enum e_docDepth
	{
		not_in_doc = -1,
		document_content,
		body,
		spreadsheet,
		table,
		table_row,
		table_cell,
		text			// Only for string cells.
	} DocDepth;

public:
	XmlContentsHandler(Data *dta);

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

	/**
	 * @brief resetDocument Reset level, row and column, clears data.
	 */
	void resetDocument();

	static const QString nameDocContent;
	static const QString nameBody;
	static const QString nameSpreadsheet;
	static const QString nameTable;
	static const QString nameTableRow;
	static const QString nameTableCell;
	static const QString nameText;


private:
	DocDepth _docDepth;	///< Current depth of document.
	int		_row;		///< Current row in document/table.
	int		_column;	///< Current column in document/table.
	bool	_tableRead;	///< True if first tsbale read.
	Data::XmlDatatype _lastType; ///< The last type we found in a opening tag.
	int		_colSpan;	///< Number cells this XML element spans.


	/**
	 * @brief XmlContentsHandler::setLastType Sets the lastType value, and gets value
	 * @param QValue value OUTPIT value found.
	 * @param QXmlAttributes atts Attriutes to find.
	 * @return value of lastType;
	 */
	Data::XmlDatatype _setLastType(QString &value, const QXmlAttributes &atts);

};

} // end namepsace

#endif // ODSXMLCONTENTSHANDLER_H
