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

#ifndef RIBBONBUTTONMODEL_H
#define RIBBONBUTTONMODEL_H

#include <QAbstractListModel>
#include <QStringList>
#include <QDir>

#include "modules/dynamicmodules.h"
#include "modules/analysismenumodel.h"



class RibbonButtonModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(bool		enabled			READ enabled			WRITE setEnabled			NOTIFY enabledChanged)
	Q_PROPERTY(bool		requiresDataset	READ requiresDataset	WRITE setRequiresDataset	NOTIFY requiresDatasetChanged)
	Q_PROPERTY(bool		isDynamic		READ isDynamic			WRITE setIsDynamic			NOTIFY isDynamicChanged)
	Q_PROPERTY(QString	title			READ titleQ				WRITE setTitleQ				NOTIFY titleChanged)
	Q_PROPERTY(QString	moduleName		READ moduleNameQ									NOTIFY moduleNameChanged)

public:
	enum {
		AnalysisMenuRole = Qt::UserRole,
		DisplayRole,
		IconSourceRole,
		EnabledAnalysisRole //For issue https://github.com/jasp-stats/INTERNAL-jasp/issues/82
	};

									RibbonButtonModel(QObject *parent, Json::Value description);
									RibbonButtonModel(QObject *parent, Modules::DynamicModule * module);

	int								rowCount(const QModelIndex &parent = QModelIndex())			const override	{	return _ribbonEntries.size();	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual	QHash<int, QByteArray>	roleNames()													const override;
	// Utility functions

	bool							requiresDataset()											const			{ return _requiresDataset;						}
	bool							isDynamic()													const			{ return _isDynamicModule;						}
	std::string						title()														const			{ return _title;								}
	QString							titleQ()													const			{ return QString::fromStdString(_title);		}
	bool							enabled()													const			{ return _enabled;								}
	std::string						moduleName()												const			{ return _moduleName;							}
	QString							moduleNameQ()												const			{ return QString::fromStdString(_moduleName);	}
	void							setRibbonEntries(Modules::RibbonEntries ribbonEntries);

public slots:
	void setRequiresDataset(bool requiresDataset);
	void setIsDynamic(bool isDynamicModule);
	void setTitle(std::string title);
	void setTitleQ(QString title)									{ setTitle(title.toStdString()); }
	void setEnabled(bool enabled);
	void setModuleName(std::string moduleName);
	void setModuleNameQ(QString moduleName)							{ setModuleName(moduleName.toStdString()); }
	void somePropertyChanged()										{ emit iChanged(this); }
	void setDynamicModules(DynamicModules * dynamicModules)			{ _dynamicModules = dynamicModules; }


signals:
	void enabledChanged();
	void requiresDatasetChanged();
	void isDynamicChanged();
	void titleChanged();
	void moduleNameChanged();
	void iChanged(RibbonButtonModel * me);


private:
	void bindYourself();

private:
	Modules::RibbonEntries			_ribbonEntries;
	AnalysisMenuModels				_analysisMenuModels;

	bool							_requiresDataset	= true,
									_isDynamicModule	= true,
									_enabled			= false;
	std::string						_title				= "",
									_moduleName			= "";
	DynamicModules				*	_dynamicModules		= nullptr;
};


#endif
