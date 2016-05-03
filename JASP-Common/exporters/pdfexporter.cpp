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

#include "pdfexporter.h"
#include "utils.h"
#include <QFile>
#include <QTextDocument>
#include <QPrinter>
#include <QWebView>

void PDFExporter::saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback)
{
	QString htmlContent = QString::fromStdString(package->analysesHTML);

	//Next code could be a hack to show plots in pdf
	//QUrl url = QUrl::fromLocalFile(QDir::current().absoluteFilePath("htmloutput.html"));
	//QUrl url = QUrl::fromLocalFile(_transferFile);
	//QWebView wdocument;
	//wdocument.setHtml(htmlContent, url); // str1 is the html file stored as QString.

	QTextDocument *document = new QTextDocument();
	document->setHtml(htmlContent);
	QPrinter printer(QPrinter::PrinterResolution);
	printer.setPaperSize(QPrinter::A4);
	printer.setOutputFormat(QPrinter::PdfFormat);
	printer.setOutputFileName(QString::fromStdString(path));
	document->print(&printer);
	delete document;

	progressCallback("Export pdf Set", 100);

}


