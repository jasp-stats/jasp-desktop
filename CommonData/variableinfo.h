//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef VARIABLEINFO_H
#define VARIABLEINFO_H

#include <QVariant>
#include <QPointer>
#include <QIcon>
#include <QAbstractItemModel>
#include <QQmlContext>
#include "columntype.h"

class VariableInfoProvider;
class ColumnEncoder;
class DataSet;
class Column;

// The Provider/Consumer mechanism makes an interface so that the consumers (the QML models) get their data without having to know how the Provider furnishes this data
// Typically, for a JASP application, the Provider will be the ColumnsModel, but if the QML forms are used somewhere else, another Provider should be instantiated.
// We need a QObject class so that we can use the Qt signals.
// The VariableInfoProvider and VariableInfoConsumer are classes used by ColumnsModel, ListModel or SourceItem, that are already QObject classes.
// As a class cannot derive from 2 QObject classes, the VariableInfoProvider and VariableInfoConsumer cannot be QObject classes.
// So we just use the VariableInfo class (which is a singleton intialized at the start of the application) to propagate the signals
class VariableInfo : public QObject
{
	Q_OBJECT
	
public:
	VariableInfo(VariableInfoProvider* provider = nullptr, QObject * parent = nullptr);
	~VariableInfo();

	Q_PROPERTY(int	rowCount		READ rowCount		NOTIFY rowCountChanged		)
	Q_PROPERTY(int	variableCount	READ variableCount	NOTIFY variableCountChanged	)
	Q_PROPERTY(bool	dataAvailable	READ dataAvailable	NOTIFY dataAvailableChanged	)

	VariableInfoProvider	*	provider()	{ return _provider; }
	

	int							rowCount();
	int							variableCount();
	bool						dataAvailable();
	DataSet					*	dataSet();
	
public slots:
	void						setProvider(VariableInfoProvider * provider);
	
signals:
	void	refresh();
	void	variableNamesChanged(		QMap<QString, QString> changedNames);
	void	filterChanged();
	void	labelsChanged(		QString columnName, QMap<QString, QString> changedLabels);
	void	variablesChanged(	QStringList changedColumns);
	void	dataSetChanged();
	void	rowCountChanged();
	void	variableCountChanged();
	void	variableTypeChanged(QString columnName, columnType colType);
	void	labelsReordered(	QString columnName);
	void	dataAvailableChanged();

private:	
	VariableInfoProvider *	_provider	= nullptr;
};

class VarInfoSignaller : public QObject
{
	Q_OBJECT
	
public:
	VarInfoSignaller(QObject * parent = nullptr) : QObject(parent) {}
	
public slots:
	void	labelChanged(const Column * column, QString orgLabel, QString newLabel);
	
signals:
	void	refresh();
	void	variableNamesChanged(		QMap<QString, QString> changedNames);
	void	filterChanged();
	void	labelsChanged(				QString columnName, QMap<QString, QString> changedLabels);
	void	variablesChanged(			QStringList changedColumns);
	void	dataSetChanged();
	void	rowCountChanged();
	void	variableCountChanged();
	void	labelsReordered(			QString columnName);
	void	variableTypeChanged(		QString variableName, columnType variableType);
	void	dataAvailableChanged();
};

class VariableInfoProvider
{ 
public:
	
	VariableInfoProvider(QObject * parent = nullptr);
	
	virtual QVariant				provideInfo(varInfoType info, const QString& name = "", int row = 0)			const	= 0;
	virtual bool					absorbInfo(	varInfoType info, const QString& name,		int row, QVariant value)		= 0;
	virtual QAbstractItemModel*		providerModel()																			{ return nullptr;			}
	/// The ColumnEncoder for the data this provider serves, or nullptr if there is none. The desktop
	/// uses this to en-/decode column names against the dataset's own encoder instead of the
	/// process-global current-encoder (which is only meaningful inside the engine's request context).
	virtual ColumnEncoder			*	columnEncoder()																{ return nullptr;			}
	
	VarInfoSignaller * infoSignaller() { return _infoSignaller; }
	
private:
	
	VarInfoSignaller * _infoSignaller = nullptr;
};

class VariableInfoConsumer
{
public:
	VariableInfoConsumer(VariableInfo * info = nullptr) : _varInfo(info) {}

	QVariant				requestInfo(varInfoType info, const QString &name = "", int row = 0)			const	{ return _varInfo && _varInfo->provider() ? _varInfo->provider()->provideInfo(info, name, row)		: QVariant();	}
	bool					sendInfo(varInfoType info, const QString &name, int row, QVariant value)		const	{ return _varInfo && _varInfo->provider() ? _varInfo->provider()->absorbInfo(info, name, row, value)	: false;	}
	bool					isInfoProviderModel(QObject* model)												const	{ return _varInfo && _varInfo->provider() ? model == _varInfo->provider()->providerModel()			: false;		}
	QAbstractItemModel*		infoProviderModel()																		{ return _varInfo && _varInfo->provider() ? _varInfo->provider()->providerModel()						: nullptr;		}

	VariableInfo*			varInfo()	const	{ return _varInfo; }
	void					setVarInfo(VariableInfo * info) { _varInfo = info; }

private:
	///A QPointer and not a raw one, because we do not own this. AnalysisForm parents a VariableInfo to
	///itself and hands it to every control on the form, and QObject destroys its children in the order
	///they were added - so that VariableInfo, made in the form's constructor, dies before the controls
	///that consume it. Their destructors still run (cleanUp() -> disconnectModels()), and a raw pointer
	///there is a use-after-free. A QPointer nulls itself instead, which every accessor above handles.
	QPointer<VariableInfo>	_varInfo = nullptr;
};

#endif // VARIABLEINFO_H

