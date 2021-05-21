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

///
/// This base model holds the RibbonButtons that are currently active and inactive in JASP and makes them available as a list
/// 
class RibbonModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int	highlightedModuleIndex	READ highlightedModuleIndex WRITE setHighlightedModuleIndex NOTIFY highlightedModuleIndexChanged)
	Q_PROPERTY(int	currentRow				READ currentRow				WRITE setCurrentRow				NOTIFY currentRowChanged			)
	Q_PROPERTY(bool dataMode				READ dataMode												NOTIFY dataModeChanged				)

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

	enum class RowType { Analyses = 0, Data};

	RibbonModel();
	~RibbonModel() { _singleton = nullptr; } 
	
	static RibbonModel * singleton() { return _singleton; }
	

	int								rowCount(const QModelIndex & = QModelIndex())				const override	{	return int(_buttonNames[_currentRow].size());	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual QHash<int, QByteArray>	roleNames()													const override;

	void						loadModules(std::vector<std::string> commonModulesToLoad = {}, std::vector<std::string> extraModulesToLoad = {});

	void						addSpecialRibbonButtonsEarly();
	void						addSpecialRibbonButtonsLate();
	Q_INVOKABLE void			showData()			{ setCurrentRow(int(RowType::Data));		}
	Q_INVOKABLE void			showStatistics()	{ setCurrentRow(int(RowType::Analyses));	}
	void						genShowEmptyData()	{ generateEmptyData(); showData(); }

	void						removeRibbonButtonModel(std::string moduleName);


	bool						isModuleName(std::string name)						const	{ return _buttonModelsByName.count(name) > 0; }
	QString						moduleName(size_t index)							const	{ return QString::fromStdString(_buttonNames[_currentRow][index]);}
	RibbonButton*				ribbonButtonModelAt(size_t index)					const	{ return ribbonButtonModel(		_buttonNames[_currentRow][index]); }
	RibbonButton*				ribbonButtonModel(std::string moduleName)			const;
	int							ribbonButtonModelIndex(RibbonButton * model)		const;

	Q_INVOKABLE void			toggleModuleEnabled(int ribbonButtonModelIndex);
	Q_INVOKABLE void			setModuleEnabled(int ribbonButtonModelIndex, bool enabled);
	QStringList					getModulesEnabled() const;

	int							highlightedModuleIndex()							const { return _highlightedModuleIndex; }
	int							currentRow()										const { return _currentRow;				}
	bool						dataMode()											const {	return _currentRow == size_t(RowType::Data);	}
	Modules::AnalysisEntry*		getAnalysis(std::string moduleName,					const std::string & analysisName);

	std::string					getModuleNameFromAnalysisName(const std::string & analysisName);




signals:
				void currentButtonModelChanged();
				void analysisClickedSignal(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module);
				void highlightedModuleIndexChanged(int highlightedModuleIndex);
				void showRCommander();
				void invalidateFilterModel();
				void currentRowChanged();
				void dataLoadedChanged(bool loaded);
				void generateEmptyData();
				void dataModeChanged(bool dataMode);
				void startExternalEdit();
				void stopExternalEdit();

				void resizeData();
				void finishCurrentEdit();


public slots:
	void addRibbonButtonModelFromDynamicModule(Modules::DynamicModule * module);
	void removeDynamicRibbonButtonModel(QString moduleName)				{ removeRibbonButtonModel(moduleName.toStdString());				}
	void setHighlightedModuleIndex(int highlightedModuleIndex);
	void moduleLoadingSucceeded(const QString & moduleName);
	void analysisClicked(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module);
	void setCurrentRow(int currentRow);

private slots:
	void dynamicModuleChanged(	Modules::DynamicModule * module);
	void dynamicModuleReplaced(	Modules::DynamicModule * oldModule, Modules::DynamicModule * module);
	void ribbonButtonModelChanged(RibbonButton* model);

private: // functions
	void addRibbonButtonModel(RibbonButton* model, size_t row);

private: // fields
	std::map<std::string, RibbonButton*>	_buttonModelsByName;
	std::vector<stringvec>					_buttonNames; //Can be multiple rows, originally [ { Analyses }, { Data Mode } ]
	int										_highlightedModuleIndex = -1;
	std::vector<std::string>				_commonModulesToLoad;
	size_t									_currentRow				= size_t(RowType::Analyses);
	
	static RibbonModel * _singleton;
};


#endif  // RIBBONMODEL_H
