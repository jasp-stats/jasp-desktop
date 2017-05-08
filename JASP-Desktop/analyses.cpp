//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "analyses.h"

#include "analysisloader.h"
#include "boost/foreach.hpp"
#include "appdirs.h"
#include "processinfo.h"

#include <QFile>
#include <QTimer>

#include "utils.h"
#include "tempfiles.h"

using namespace std;

Analyses::Analyses()
{
	_nextId = 0;

	//QTimer *timer = new QTimer(this);
	//timer->setInterval(5000);
	//
	//QObject::connect(timer, SIGNAL(timeout()), this, SLOT(flushDefaultsToDisk()));
	//
	//timer->start();
}

Analysis *Analyses::create(const QString &name, Json::Value *optionsData, Analysis::Status status)
{
	return create(name, _nextId++, optionsData, status);
}

Analysis *Analyses::create(const QString &name, int id, Json::Value *options, Analysis::Status status)
{
	if (id >= _nextId)
		_nextId = id + 1;

	Analysis *analysis = AnalysisLoader::load(id, name.toStdString(), options);
	analysis->setStatus(status);

	if (options == NULL)
		assignDefaults(analysis);

	while (id >= _analyses.size())
		_analyses.push_back(NULL);

	_analyses[id] = analysis;

	analysis->optionsChanged.connect(boost::bind(&Analyses::analysisOptionsChangedHandler, this, _1));
	analysis->toRefresh.connect(boost::bind(&Analyses::analysisToRefreshHandler, this, _1));
	analysis->saveImage.connect(boost::bind(&Analyses::analysisSaveImageHandler, this, _1, _2));
	analysis->imageSaved.connect(boost::bind(&Analyses::analysisImageSavedHandler, this, _1));
	analysis->resultsChanged.connect(boost::bind(&Analyses::analysisResultsChangedHandler, this, _1));
	analysis->userDataLoaded.connect(boost::bind(&Analyses::analysisUserDataLoadedHandler, this, _1));

	analysisAdded(analysis);

	return analysis;
}

void Analyses::clear()
{
	for (Analyses::iterator itr = this->begin(); itr != this->end(); itr++)
	{
		Analysis *analysis = *itr;
		delete analysis;
	}

	_analyses.clear();
	_defaults.clear();
}

Analysis *Analyses::get(int id) const
{
	if (id < _analyses.size())
		return _analyses.at(id);
	else
		return NULL;
}

QList<Analysis*>::iterator Analyses::begin()
{
	return _analyses.begin();
}

QList<Analysis*>::iterator Analyses::end()
{
	return _analyses.end();
}

int Analyses::count() const
{
	int c = 0;

	BOOST_FOREACH(Analysis *analysis, _analyses)
	{
		if (analysis != NULL)
			c++;
	}

	return c;
}

void Analyses::flushDefaultsToDisk()
{
	QString path = AppDirs::analysisDefaultsDir();

	BOOST_FOREACH (Defaults &defaults, _defaults)
	{
		if (defaults.needsSync)
		{
			std::string json = defaults.options->asJSON(false).toStyledString();

			QString fileName = QString("%1/%2.json").arg(path).arg(defaults.analysisName);
			QString tmpFileName = QString("%1.%2").arg(fileName).arg(ProcessInfo::currentPID());

			QFile file(tmpFileName);

			file.open(QIODevice::WriteOnly | QIODevice::Truncate);
			file.write(json.c_str(), json.length());
			file.close();

			if (Utils::renameOverwrite(tmpFileName.toStdString(), fileName.toStdString()))
				defaults.needsSync = false;
			else
				file.remove();
		}
	}
}

void Analyses::assignDefaults(Analysis *analysis)
{
	QString name = QString::fromStdString(analysis->name());

	if (_defaults.contains(name))
	{
		analysis->options()->set(_defaults[name].options->asJSON());
	}
	/*else
	{
		QString path = AppDirs::analysisDefaultsDir() + "/" + name + ".json";
		QFile file(path);

		bool defaultsExist = file.exists();

		if (defaultsExist)
		{
			file.open(QFile::ReadOnly);
			QByteArray contents = file.readAll();
			Json::Reader reader;
			Json::Value root;

			if (reader.parse(contents.constData(), contents.constData() + contents.length(), root, false))
				analysis->options()->set(root);
		}

		Defaults defs;
		defs.analysisName = name;
		defs.options = static_cast<Options*>(analysis->options()->clone());

		if (defaultsExist == false)
		{
			defs.needsSync = true;
			_defaults[name] = defs;
			flushDefaultsToDisk();
		}
		else
		{
			defs.needsSync = false;
			_defaults[name] = defs;
		}

	}*/
}

void Analyses::analysisUserDataLoadedHandler(Analysis *analysis)
{
	analysisUserDataLoaded(analysis);
}

void Analyses::analysisResultsChangedHandler(Analysis *analysis)
{
	analysisResultsChanged(analysis);
}

void Analyses::analysisImageSavedHandler(Analysis *analysis)
{
	analysisImageSaved(analysis);
}

void Analyses::analysisOptionsChangedHandler(Analysis *analysis)
{
	QString name = QString::fromStdString(analysis->name());

	if (_defaults.contains(name))
	{
		Defaults &defaults = _defaults[name];
		defaults.options->set(analysis->options()->asJSON());
		defaults.needsSync = true;
	}
	else
	{
		Defaults defaults;

		defaults.analysisName = name;
		defaults.options = static_cast<Options*>(analysis->options()->clone());
		defaults.needsSync = true;
		_defaults[name] = defaults;
	}

	analysisOptionsChanged(analysis);
}

void Analyses::analysisToRefreshHandler(Analysis *analysis)
{
	analysis->setStatus(Analysis::Empty);
	tempfiles_deleteAll(analysis->id());
	analysisToRefresh(analysis);
}
void Analyses::analysisSaveImageHandler(Analysis *analysis, Json::Value &options)
{
	analysis->setStatus(Analysis::SaveImg);
	analysis->setSaveImgOptions(options);
	analysisSaveImage(analysis);
}


