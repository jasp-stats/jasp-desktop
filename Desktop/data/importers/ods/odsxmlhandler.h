//
// Copyright (C) 2016 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//


#ifndef __ODSXMLERRORHANDLER_H_
#define __ODSXMLERRORHANDLER_H_

#include <QXmlDefaultHandler>

//#include "odsdata.h"
#include "odsimportdataset.h"

namespace ods
{

class XmlHandler : public QXmlDefaultHandler
{
public:
	/**
	 * @brief OdsXmlHandler Ctor
	 * @param data A
	 */
//	XmlHandler(Data *data);
	XmlHandler(ODSImportDataSet *data);
	virtual ~XmlHandler();

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
	virtual bool startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &atts) = 0;

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
	virtual bool endElement(const QString &namespaceURI, const QString &localName, const QString &qName) = 0;

	/**
	 * @brief characters Called when char data found.
	 * @param ch The found data.
	 * @return true on no error.
	 */
	virtual bool characters(const QString &ch) = 0;

protected:
	ODSImportDataSet * _dataSet;
};

}

#endif // ODSXMLERRORHANDLER_H
