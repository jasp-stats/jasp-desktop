#ifndef ANALYSISBASE_H
#define ANALYSISBASE_H

#include <QObject>
#include <QPointer>
#include <json/json.h>
#include "controls/jaspcontrol.h"
#include "appinfo.h"


class AnalysisForm;
class DataSet;
class Filter;

class AnalysisBase : public QObject
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY(QQuickItem		*	formItem				READ formItem										NOTIFY formItemChanged			)
	Q_PROPERTY(QString				qmlError				READ qmlError			WRITE setQmlError			NOTIFY qmlErrorChanged			)
	Q_PROPERTY(Filter * filter		READ filter								NOTIFY filterChanged) //Select filter by changing filterName
	Q_PROPERTY(QString	filterName	READ filterName							NOTIFY filterChanged)
	Q_PROPERTY(int		filterId	READ filterId		WRITE setFilterId	NOTIFY filterChanged)
	Q_PROPERTY(QString	dataSpec	READ dataSpec							NOTIFY dataSpecChanged)
	

public:
	explicit AnalysisBase(QObject *parent = nullptr);
	AnalysisBase(QObject *parent, AnalysisBase* duplicateMe);

	virtual				bool				isOwnComputedColumn(const std::string &col)					const	{ return false; }
	virtual				void				refresh()															{}
	virtual				void				run()																{}
	virtual				void				reloadForm()														{}
	virtual				void				exportResults()														{}
	virtual				bool				isDuplicate()												const	{ return false;				}
	virtual				bool				wasUpgraded()												const	{ return false;				}
	virtual				bool				needsRefresh()												const	{ return false;				}
	virtual				const std::string   module()													const	{ return emptyString;		}
	virtual				const std::string & name()														const	{ return emptyString;		}
	virtual				const std::string & title()														const	{ return emptyString;		}
	virtual				const std::string & titleDefault()												const	{ return emptyString;		}
	virtual				void				setTitle(const std::string& titel)									{}
	virtual				void				preprocessMarkdownHelp(const QString& md)					const	{}
	virtual				QString				helpFile()															{ return "";				}
	virtual				const stringvec   & upgradeMsgsForOption(const std::string& name)				const	{ return emptyStringVec;	}
	virtual				const Json::Value & resultsMeta()												const 	{ return Json::Value::null;	}
	virtual				const Json::Value & getRSource(const std::string& name)							const 	{ return Json::Value::null;	}
	virtual				void				initialized(AnalysisForm* form, bool isNewAnalysis)					{}
	virtual				std::string			qmlFormPath(bool addFileProtocol = true,
											bool ignoreReadyForUse = false)								const;
	virtual Q_INVOKABLE	QString				helpFile()													const	{ return ""; }
	virtual Q_INVOKABLE void				createForm(QQuickItem* parentItem=nullptr);
	virtual				void				destroyForm();
	virtual				bool				isColumnFreeOrMine(const QString & columnName)				const	{ return false; }
	virtual				DataSet *			dataSet()													const	{ return nullptr; }

	virtual QVariant			getConstant(const QString& key, const QVariant& defaultValue)													const	{ return defaultValue;		}
	virtual QVariant			getConstant(const QString& key, const QVariant& defaultValue, const QString& module, const QString& analysis)	const	{ return defaultValue;		}
	virtual bool				optionLocked(const QString& name)																				const	{ return false; };
	virtual	const Version	  &	moduleVersion()																									const	{ return AppInfo::version;	}

						const Json::Value &	boundValues()												const	{ return _boundValues;		}
						const Json::Value &	boundValue(const std::string& name,
														 const QVector<JASPControl::ParentKey>& parentKeys = {});

						void				setBoundValue(const std::string& name, const Json::Value& value, const Json::Value& meta, const QVector<JASPControl::ParentKey>& parentKeys = {});
						void				setBoundValues(const Json::Value& boundValues);
						const Json::Value	optionsMeta()												const	{ return _boundValues.get(".meta", Json::nullValue);	}
						void				clearBoundValues()													{ _boundValues.clear();		}


						QQuickItem		  *	formItem()													const;

						const QString	  &	qmlError()													const;
						void				setQmlError(const QString &newQmlError);
						void				sendRScript(const QString & script, const QString & controlName, bool whiteListedVersion)		{ emit sendRScriptSignal(script, controlName, whiteListedVersion, tq(module())); }
						void				sendFilter(	const QString & name)																{ emit sendFilterSignal(name, tq(module())); }
							
						Filter			*	filter() const;
						
						///Whether this analysis operates on the given dataset. An analysis without an explicit dataset
						///binding (e.g. reports, or before a dataset is selected) is treated as using any dataset.
						bool				usesDataSet(int dataSetId)						const;

						QString				filterName()	const;
						int					filterId()		const;
						void				setFilterId(int filterId);

						///The dataset and/or filter this analysis runs on, but only insofar as they actually tell it
						///apart from the others: empty when there is but a single dataset holding a single filter.
						///This is a read-only decoration of the title and never becomes part of the title itself.
						QString				dataSpec()		const;

						bool				isAnnotated()		const	{ return _isAnnotated; }
						void				setIsAnnotated(bool isAnnotated);
	


public slots:
	virtual void	boundValueChangedHandler()																	{}
	virtual void	requestColumnCreationHandler(			const std::string & columnName, columnType colType)	{}
	virtual void	requestComputedColumnCreationHandler(	const std::string & columnName)						{}
	virtual void	requestComputedColumnDestructionHandler(const std::string & columnName)						{}
	virtual void	onUsedVariablesChanged()																	{}


signals:
	void			sendRScriptSignal(QString script, QString controlName, bool whiteListedVersion, QString module);
	void			sendFilterSignal( QString  name,  QString module);
	void			formItemChanged();
	void			qmlErrorChanged();
	void			boundValuesChanged();
	void			filterChanged(Filter * f);
	void			dataSpecChanged();


protected:
	Json::Value&	_getParentBoundValue(const QVector<JASPControl::ParentKey> & parentKeys, QVector<std::string>& parentNames, bool & found, bool createAnyway = false);
	std::string		_displayParentKeys(const QVector<JASPControl::ParentKey> & parentKeys) const;
	void			connectDataSpecChanges();


	AnalysisForm*	_analysisForm		= nullptr;
	QQuickItem	*	_parentItem			= nullptr;
	QString			_qmlError;
	bool			_isAnnotated		= false;
	//Guarded pointers: a Filter/DataSet is owned by a DataSet/Workspace that may be destroyed (e.g.
	//multi-dataset teardown) while the analysis lives on; the guard auto-nulls on destruction so the
	//analysis never dereferences freed memory. _filterDataSet also derives from the (possibly null)
	//_filter instead of being tracked separately on dataset teardown.
	QPointer<Filter>				_filter				;
	QPointer<DataSet>				_filterDataSet		;

private:
	Json::Value		_boundValues		= Json::objectValue,
					_orgBoundValues		= Json::objectValue;
	
protected:
	static const std::string	emptyString; ///< Otherwise we return references to a temporary object (std::string(""))
	static const stringvec		emptyStringVec;
};

#endif // ANALYSISBASE_H
