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

#ifndef SOURCEITEM_H
#define SOURCEITEM_H

#include <QObject>
#include <QVector>
#include <QVariant>
#include <QMap>
#include <QSet>
#include <QAbstractItemModel>

#include "jasplistcontrol.h"
#include "variableinfo.h"

class SourceItem : public QObject, public VariableInfoConsumer
{
	Q_OBJECT
public:

	struct ConditionVariable
	{
		QString name,
				controlName,
				propertyName;
		bool	addQuotes = false;
		ConditionVariable(const QString& _name, const QString& _controlName, const QString& _propertyName, bool _addQuotes = false)
			: name(_name), controlName(_controlName), propertyName(_propertyName), addQuotes(_addQuotes) {}
		ConditionVariable(const ConditionVariable& source)
			: name(source.name), controlName(source.controlName), propertyName(source.propertyName), addQuotes(source.addQuotes) {}
		ConditionVariable() {}
	};

	struct SourceValuesItem
	{
		QString label, value, info;
		SourceValuesItem(const QString& l, const QString& v, const QString& i) : label{l}, value{v}, info{i} {}

	};

	static const QString SourceValueLabel, SourceValueValue, SourceValueInfo;
	typedef QVector<SourceValuesItem> SourceValuesType;

	SourceItem(
			  JASPListControl* targetListControl
			, QMap<QString, QVariant>& map
			, const SourceValuesType& values
			, const QVector<SourceItem*> rSources
			, QAbstractItemModel* nativeModel = nullptr
			, const QVector<SourceItem*>& discardSources = QVector<SourceItem*>()
			, const QVector<QMap<QString, QVariant> >& conditionVariables = QVector<QMap<QString, QVariant> >()
			);

	SourceItem(JASPListControl* _listControl, const SourceValuesType& _values);

	SourceItem(JASPListControl* _listControl, const QString& sourceName, const QString& sourceUse);

	SourceItem(JASPListControl* _listControl = nullptr);

	ListModel*				sourceListModel()					{ return _sourceListModel;			}
	const QString&			rowControlName()			const	{ return _rowControlName;			}
	const QStringList&		sourceFilter()				const	{ return _sourceFilter;					}
	bool					combineWithOtherModels()	const	{ return _combineWithOtherModels;	}
	bool					generateInteractions()		const	{ return _combineWithOtherModels || (_combineTerms != JASPControl::CombinationType::NoCombination); }
	bool					isAnalysisDataSet()			const	{ return _isDataSetVariables;		}
	bool					isNativeModel()				const	{ return _sourceNativeModel != nullptr;	}
	QAbstractItemModel*		nativeModel()						{ return _sourceNativeModel;				}
	Terms					getTerms();
	QSet<QString>			usedControls()				const;


	void										connectModels();
	void										disconnectModels();
	static QVector<SourceItem*>					readAllSources(JASPListControl* _listControl);
	static QList<QVariant>						getListVariant(QVariant var);
	static Terms								filterTermsWithCondition(ListModel* model, const Terms& terms, const QString& condition, const QVector<ConditionVariable>& conditionVariables = {}, const QMap<QString, QStringList> &termsMap = {});


private:
	static QString							_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse);
	static QString							_readRSourceName(const QString& sourceNameExt, QString& sourceUse);
	static QMap<QString, QVariant>			_readSource(JASPListControl* _listControl, const QVariant& source, SourceValuesType& sourceValues, QVector<SourceItem*>& rSources, QAbstractItemModel*& _nativeModel);
	static SourceItem*						_readRSource(JASPListControl* listControl, const QVariant& rSource);
	static SourceValuesType					_readValues(JASPListControl* _listControl, const QVariant& _values);

	void									_setUp();
	Terms									_readAllTerms();

private slots:
	void									_resetModel();
	void									_dataChangedHandler(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles = QVector<int>());
	void									_rSourceChanged(const QString& name);

private:
	JASPListControl		*			_targetListControl			= nullptr;
	QString							_sourceName,
									_rowControlName;
	QStringList						_sourceFilter;
	QVector<SourceItem*>			_discardSources;
	QVector<SourceItem*>			_rSources;
	SourceValuesType				_values;
	bool							_isValuesSource				= false;
	bool							_isRSource					= false;
	ListModel			*			_sourceListModel			= nullptr;
	QAbstractItemModel	*			_sourceNativeModel			= nullptr;
	int								_nativeModelRole			= Qt::DisplayRole;
	bool							_isDataSetVariables			= false,
									_combineWithOtherModels		= false,
									_noInteractions				= false;
	QString							_conditionExpression;
	QVector<ConditionVariable>		_conditionVariables;
	bool							_connected					= false;
	JASPControl::CombinationType	_combineTerms				= JASPControl::CombinationType::NoCombination;
	int								_onlyTermsWithXComponents	= 0;
};

#endif // SOURCEITEM_H
