#ifndef ANALYSISBASE_H
#define ANALYSISBASE_H

#include <QObject>
#include <json/json.h>
#include "controls/jaspcontrol.h"
#include "appinfo.h"

class AnalysisForm;

class AnalysisBase : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QQuickItem		*	formItem				READ formItem										NOTIFY formItemChanged			)
	Q_PROPERTY(QString				qmlError				READ qmlError			WRITE setQmlError			NOTIFY qmlErrorChanged			)

public:
	explicit AnalysisBase(QObject *parent = nullptr, Version moduleVersion = AppInfo::version);
	AnalysisBase(QObject *parent, AnalysisBase* duplicateMe);

	virtual bool isOwnComputedColumn(const std::string &col)				const	{ return false; }
	virtual void refresh()															{}
	virtual void run()																{}
	virtual void reloadForm()														{}
	virtual void exportResults()													{}
	virtual bool isDuplicate()												const	{ return false;				}
	virtual bool wasUpgraded()												const	{ return false;				}
	virtual bool needsRefresh()												const	{ return false;				}
	virtual const std::string & module()									const	{ return emptyString;		}
	virtual const std::string & name()										const	{ return emptyString;		}
	virtual const std::string & title()										const	{ return emptyString;		}
	virtual void setTitle(const std::string& titel)									{}
	virtual void preprocessMarkdownHelp(const QString& md)					const	{}
	virtual QString helpFile()														{ return "";				}
	virtual const stringvec & upgradeMsgsForOption(const std::string& name) const	{ return emptyStringVec;	}
	virtual const Json::Value & resultsMeta()								const 	{ return Json::Value::null;	}
	virtual const Json::Value & getRSource(const std::string& name)			const 	{ return Json::Value::null;	}
	virtual void initialized(AnalysisForm* form, bool isNewAnalysis)				{}
	virtual std::string	qmlFormPath(bool addFileProtocol = true, 
									bool ignoreReadyForUse = false)			const;

	virtual Q_INVOKABLE	QString	helpFile()									const	{ return ""; }
	virtual Q_INVOKABLE void	createForm(QQuickItem* parentItem=nullptr);
	virtual				void	destroyForm();

	const Json::Value&	boundValues()										const	{ return _boundValues;		}
	const Json::Value&	orgBoundValues()									const	{ return _orgBoundValues;	}
	const Json::Value&	boundValue(const std::string& name, const QVector<JASPControl::ParentKey>& parentKeys = {});

	void				setBoundValue(const std::string& name, const Json::Value& value, const Json::Value& meta, const QVector<JASPControl::ParentKey>& parentKeys = {});
	void				setBoundValues(const Json::Value& boundValues);
	void				setOrgBoundValues(const Json::Value& orgBoundValues)		{ _orgBoundValues = orgBoundValues; }
	const	Json::Value	optionsMeta()										const	{ return _boundValues.get(".meta", Json::nullValue);	}
	void				clearOptions()												{ _boundValues.clear();		}

	const	Version	&	moduleVersion()										const	{ return _moduleVersion;	}

	QQuickItem *	formItem()												const;

	const QString &	qmlError()																	const;
	void			setQmlError(const QString &newQmlError);
	void			sendRScript(QString script, QString controlName, bool whiteListedVersion)		{ emit sendRScriptSignal(script, controlName, whiteListedVersion, tq(module())); }


public slots:
	virtual void	boundValueChangedHandler()														{}
	virtual void	requestColumnCreationHandler(const std::string&columnName, columnType colType)	{}
	virtual void	requestComputedColumnCreationHandler(const std::string& columnName)				{}
	virtual void	requestComputedColumnDestructionHandler(const std::string& columnName)			{}

signals:
	void			sendRScriptSignal(QString script, QString controlName, bool whiteListedVersion, QString module);
	void			formItemChanged();
	void			qmlErrorChanged();
	void			boundValuesChanged();


protected:
	Json::Value&	_getParentBoundValue(const QVector<JASPControl::ParentKey> & parentKeys, QVector<std::string>& parentNames, bool & found, bool createAnyway = false);


	AnalysisForm*	_analysisForm		= nullptr;
	QQuickItem	*	_parentItem			= nullptr;
	QString			_qmlError;
	Version			_moduleVersion;

private:
	Json::Value		_boundValues		= Json::objectValue,
					_orgBoundValues		= Json::objectValue;



protected:
	static const std::string	emptyString; ///< Otherwise we return references to a temporary object (std::string(""))
	static const stringvec		emptyStringVec;
};

#endif // ANALYSISBASE_H
