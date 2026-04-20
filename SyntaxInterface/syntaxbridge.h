//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef SYNTAXBRIDGE_H
#define SYNTAXBRIDGE_H

#include <QObject>
#include <QQuickItem>
#include <QQmlApplicationEngine>
#include <QString>
#include "version.h"

class AnalysisForm;

struct AnalysisInfo
{
	QString		analysisName, qmlFileName, analysisTitle;
	bool		preloadData = false, hasWrapper = false;

	AnalysisInfo(const QString & _analysisName, const QString & _qmlFileName, const QString & _analysisTitle, bool _preloadData, bool _hasWrapper)
		: analysisName{_analysisName}, qmlFileName{_qmlFileName}, analysisTitle{_analysisTitle}, preloadData{_preloadData}, hasWrapper{_hasWrapper} {}
};

struct ModuleInfo
{
	QString						name, title, author, website, license, maintainer, description;
	bool						requiresData = false, isCommon = false, hasWrappers = false;
	Version						version;
	std::vector<AnalysisInfo>	analyses;

	ModuleInfo() {}
	ModuleInfo(const QString & _name, const QString & _title, const QString & _author, const QString & _website, const QString & _license, const QString & _maintainer, const QString & _description,
			   bool _requiresData, bool _isCommon, bool _hasWrappers, const Version & _version)
		: name{_name}, title{_title}, author{_author}, website{_website}, license{_license}, maintainer{_maintainer}, description{_description},
		requiresData{_requiresData}, isCommon{_isCommon}, hasWrappers{_hasWrappers}, version{_version} {}
};

void				blockSignalsRecursive(	QObject* item);
void				deleteQuickItem(		QQuickItem* item);
void				sendMessage(			const char * msg);
bool				init(					bool dbInMemory = false);
void				sendRScriptHandler(		AnalysisForm* form, QString script, QString controlName, bool whiteListedVersion);
AnalysisForm*		getQmlForm(				const QString & qmlFileStr);
bool				generateWrapper(		const QString & modulePath, const QString & analysisName, const QString & qmlFileName, const QString & analysisTitle, bool preloadData);
ModuleInfo			parseDescription(		const QString & modulePath);

#endif // SYNTAXBRIDGE_H
