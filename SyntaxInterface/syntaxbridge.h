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

class AnalysisForm;

struct AnalysisInfo
{
	QString analysisName, qmlFileName, analysisTitle;
	AnalysisInfo(const QString& _analysisName, const QString& _qmlFileName, const QString& _analysisTitle)
		: analysisName{_analysisName}, qmlFileName{_qmlFileName}, analysisTitle{_analysisTitle} {}
};

void				blockSignalsRecursive(	QObject* item);
void				deleteQuickItem(		QQuickItem* item);
void				sendMessage(			const char * msg);
bool				init(					bool dbInMemory = true);
void				sendRScriptHandler(		AnalysisForm* form, QString script, QString controlName, bool whiteListedVersion);
AnalysisForm*		getQmlForm(				const QString& qmlFileStr);
bool				generateWrapper(		const QString& modulePath, const QString& analysisName, const QString& qmlFileName, const QString& analysisTitle, bool preloadData);


#endif // SYNTAXBRIDGE_H
