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
#include <QFile>
#include <QTextDocument>
#include <fstream>
#include "utilenums.h"
#include "results/resultsjsinterface.h"
#include <QThread>
#include "log.h"
#include <fstream>


ResultExporter::ResultExporter()
{
	_defaultFileType = Utils::FileType::html;
	_allowedFileTypes.push_back(Utils::FileType::html);
	_allowedFileTypes.push_back(Utils::FileType::pdf);
}


//waiting is an anti pattern for async stuff like this. But since we already do it I might aswell make it even worse.
bool ResultExporter::prepareForExport()
{
	_exportPrepMutex.lock();
	QMetaObject::Connection exportPrepConnection = QObject::connect(ResultsJsInterface::singleton(), &ResultsJsInterface::exportPrepFinished, [&]()
	{
		Log::log() << "Export preparations completed, telling thread to continue!" << std::endl;
		_exportPrep.wakeAll();
	});

	Log::log() << "Thread for export preparation will wait until preparations are done." << std::endl;
	emit ResultsJsInterface::singleton()->prepForExport();

	_exportPrep.wait(&_exportPrepMutex, 5000); //wait for 5 seconds max
	_exportPrepMutex.unlock();
	QObject::disconnect(exportPrepConnection);
	Utils::sleep(200); //why? because the webview lies and says some script are done but while on some systems they are not.
	return true;
}

void ResultExporter::saveDataSet(const std::string &path, boost::function<void(int)> progressCallback)
{
	//set the needed settings and wait for their application to be finished
	prepareForExport();

	if (_currentFileType == Utils::FileType::pdf)
	{
		_writingToPdfMutex.lock();

		_pdfPath = tq(path);
		progressCallback(50);

		QMetaObject::Connection printConnection = QObject::connect(ResultsJsInterface::singleton(), &ResultsJsInterface::pdfPrintingFinished, [&](QString pdfPath)
		{
			if(_pdfPath != pdfPath)
			{
				Log::log() << "Got unexpected pdfPrintingFinished event! Expected path: \"" << _pdfPath << "\" but got: \"" << pdfPath << "\"...\nIgnoring it!" << std::endl;
				return;
			}

			Log::log() << "PDF printing to : \"" << _pdfPath << "\" completed, telling thread to continue!" << std::endl;

			_writingToPdf.wakeAll();
		});

		ResultsJsInterface::singleton()->exportToPDF(_pdfPath);

		Log::log() << "Thread for exporting PDF to '" << _pdfPath << "' will wait until exporting is done.";
		_writingToPdf.wait(&_writingToPdfMutex);
		_writingToPdfMutex.unlock();

		QObject::disconnect(printConnection);

		progressCallback(100);
	}
	else
	{
		ResultsJsInterface::singleton()->exportHTML();
		DataSetPackage::pkg()->waitForExportResultsReady(); //waits for html export to be ready
		std::ofstream outfile(path.c_str(), std::ios::out);

		outfile << DataSetPackage::pkg()->analysesHTML() << std::flush;
		outfile.close();
		progressCallback(100);
	}
}
