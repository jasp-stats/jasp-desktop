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


#include "modules/ribbonbutton.h"
#include "gui/preferencesmodel.h"


class RibbonModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int highlightedModuleIndex READ highlightedModuleIndex WRITE setHighlightedModuleIndex NOTIFY highlightedModuleIndexChanged)

public:
	enum {
		ClusterRole = Qt::UserRole,
		DisplayRole,
		RibbonRole,
		EnabledRole,
		CommonRole,
		ModuleNameRole,
		ModuleTitleRole,
		ModuleRole,
		ActiveRole,
		BundledRole,
		VersionRole,
		SpecialRole
	};

	RibbonModel();
	~RibbonModel() { _singleton = nullptr; } 
	
	static RibbonModel * singleton() { return _singleton; }
	

	int								rowCount(const QModelIndex & = QModelIndex())				const override	{	return int(_moduleNames.size());	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual QHash<int, QByteArray>	roleNames()													const override;

	void						loadModules(std::vector<std::string> commonModulesToLoad = {}, std::vector<std::string> extraModulesToLoad = {});

	void						addRibbonButtonModelFromDynamicModule(Modules::DynamicModule * module);
	void						addRibbonButtonRPrompt();

	void						removeRibbonButtonModel(std::string moduleName);


	bool						isModuleName(std::string name)						const	{ return _buttonModelsByName.count(name) > 0; }
	QString						moduleName(size_t index)							const	{ return QString::fromStdString(_moduleNames[index]);}
	RibbonButton*				ribbonButtonModelAt(size_t index)					const	{ return ribbonButtonModel(_moduleNames[index]); }
	RibbonButton*				ribbonButtonModel(std::string moduleName)			const;
	int							ribbonButtonModelIndex(RibbonButton * model)		const;

	Q_INVOKABLE void			toggleModuleEnabled(int ribbonButtonModelIndex);
	Q_INVOKABLE void			setModuleEnabled(int ribbonButtonModelIndex, bool enabled);
	QStringList					getModulesEnabled() const;

	int							highlightedModuleIndex() const { return _highlightedModuleIndex; }
	Modules::AnalysisEntry*		getAnalysis(std::string moduleName, const std::string & analysisName);

	std::string					getModuleNameFromAnalysisName(const std::string & analysisName);

signals:
				void currentButtonModelChanged();
				void analysisClickedSignal(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module);
				void highlightedModuleIndexChanged(int highlightedModuleIndex);
				void analysisTitleChanged(std::string moduleName, std::string oldTitle, std::string newTitle);
				void showRCommander();

public slots:
	void addDynamicRibbonButtonModel(Modules::DynamicModule * module)	{ addRibbonButtonModelFromDynamicModule(module);		}
	void removeDynamicRibbonButtonModel(QString moduleName)				{ removeRibbonButtonModel(moduleName.toStdString());				}
	void setHighlightedModuleIndex(int highlightedModuleIndex);
	void moduleLoadingSucceeded(const QString & moduleName);
	void analysisClicked(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module);

private slots:
	void dynamicModuleChanged(Modules::DynamicModule * module);
	void ribbonButtonModelChanged(RibbonButton* model);
	void dynamicModuleReplaced(Modules::DynamicModule * oldModule, Modules::DynamicModule * module);

private: // functions
	void addRibbonButtonModel(RibbonButton* model);

private: // fields
	std::map<std::string, RibbonButton*>	_buttonModelsByName;
	std::vector<std::string>				_moduleNames;
	int										_highlightedModuleIndex = -1;
	std::vector<std::string>				_commonModulesToLoad;
	
	static RibbonModel * _singleton;
};


#endif  // RIBBONMODEL_H
