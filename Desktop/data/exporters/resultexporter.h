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

#ifndef RESULTEXPORTER_H
#define RESULTEXPORTER_H

#include "exporter.h"
#include <QMutex>
#include <QWaitCondition>

class ResultExporter: public Exporter
{
public:
	ResultExporter();
	void saveDataSet(const std::string &path, boost::function<void (int)> progressCallback) OVERRIDE;

private:
	QString			_pdfPath;
	QMutex			_writingToPdfMutex;
	QWaitCondition	_writingToPdf;


	JASPTIMER_CLASS(ResultExporter);
};

#endif // RESULTEXPORTER_H
