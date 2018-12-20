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


#ifndef RIBBONMODEL_H
#define RIBBONMODEL_H

#include <QAbstractListModel>
#include <QStringList>
#include <QDebug>

#include "modules/ribbonbuttonmodel.h"

class RibbonModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(RibbonButtonModel *currentButtonModel	READ currentButtonModel WRITE setCurrentButtonModel NOTIFY currentButtonModelChanged)

public:
	enum {
		ClusterRole = Qt::UserRole,
		DisplayRole
	};

	RibbonModel(QObject *parent) : QAbstractListModel(parent) {}

	int								rowCount(const QModelIndex & = QModelIndex())				const override	{	return int(_moduleNames.size());	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual QHash<int, QByteArray>	roleNames()													const override;

	// custom functions

	void						addRibbonButtonModelFromModulePath(QFileInfo modulePath);
	void						addRibbonButtonModelFromDynamicModule(Modules::DynamicModule * module) { addRibbonButtonModel(new RibbonButtonModel(this, module)); }

	void						removeRibbonButtonModel(std::string moduleName);

	const
	std::vector<std::string> &	moduleNames()						const	{ return _moduleNames;	}

	bool						isModuleName(std::string name)		const	{ return _modulesByName.count(name) > 0; }
	RibbonButtonModel*			ribbonButtonModel(std::string);

	void						connectToDynamicModules(DynamicModules * dynamicModules);

	RibbonButtonModel*			currentButtonModel() const							{ return _currentButtonModel; }
	void						setCurrentButtonModel(RibbonButtonModel* newModel);

	Q_INVOKABLE void			analysisClicked(QString);

signals:
	void currentButtonModelChanged();
	void analysisClickedSignal(QString analysis, QString module);

public slots:
	void addDynamicRibbonButtonModel(Modules::DynamicModule * module)	{ addRibbonButtonModelFromDynamicModule(module);	}
	void removeDynamicRibbonButtonModel(std::string moduleName)			{ removeRibbonButtonModel(moduleName);				}

private: // functions
	void										addRibbonButtonModel(RibbonButtonModel* model);

private: // fields
	std::map<std::string, RibbonButtonModel*>	_modulesByName;
	std::vector<std::string>					_moduleNames; //To keep order
	RibbonButtonModel							*_currentButtonModel = nullptr;
};


#endif  // RIBBONMODEL_H
