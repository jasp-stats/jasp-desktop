//
// Copyright (C) 2018 University of Amsterdam
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

#include "resultexporter.h"
#include "utils.h"
#include <boost/nowide/fstream.hpp>
#include <QFile>
#include <QTextDocument>
#include <QPrinter>
#include "utilenums.h"

ResultExporter::ResultExporter()
{
	_defaultFileType = Utils::FileType::html;
	_allowedFileTypes.push_back(Utils::FileType::html);
#ifdef JASP_DEBUG
	_allowedFileTypes.push_back(Utils::FileType::pdf);
#endif
}

void ResultExporter::saveDataSet(const std::string &path, boost::function<void(int)> progressCallback)
{

	int maxSleepTime = 5000;
	int sleepTime = 100;
	int delay = 0;

	while (DataSetPackage::pkg()->isReady() == false)
	{
		if (delay > maxSleepTime)
			break;

		Utils::sleep(sleepTime);
		delay += sleepTime;
	}


	if (_currentFileType == Utils::FileType::pdf)
	{
		QString htmlContent = QString::fromStdString(DataSetPackage::pkg()->analysesHTML());

		//Next code could be a hack to show plots in pdf
		//QUrl url = QUrl::fromLocalFile(QDir::current().absoluteFilePath("htmloutput.html"));
		//QUrl url = QUrl::fromLocalFile(_transferFile);
		//QWebEngineView wdocument;
		//wdocument.setHtml(htmlContent, url); // str1 is the html file stored as QString.

		QTextDocument *document = new QTextDocument();
		document->setHtml(htmlContent);
		QPrinter printer(QPrinter::PrinterResolution);
		printer.setPaperSize(QPrinter::A4);
		printer.setOutputFormat(QPrinter::PdfFormat);
		printer.setOutputFileName(QString::fromStdString(path));
		document->print(&printer);
		delete document;

	}
	else
	{
		boost::nowide::ofstream outfile(path.c_str(), std::ios::out);

		outfile << DataSetPackage::pkg()->analysesHTML() << std::flush;
		outfile.close();
	}

	progressCallback(100);
}
