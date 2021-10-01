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

class SourceItem : public QObject
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

	SourceItem(
			  JASPListControl* _listControl
			, QMap<QString, QVariant>& map
			, const JASPListControl::LabelValueMap& _values
			, const QVector<SourceItem*> _rSources
			, QAbstractItemModel* _nativeModel = nullptr
			, const QVector<SourceItem*>& _discardSources = QVector<SourceItem*>()
			, const QVector<QMap<QString, QVariant> >& _conditionVariables = QVector<QMap<QString, QVariant> >()
			);

	SourceItem(JASPListControl* _listControl, const JASPListControl::LabelValueMap& _values);

	SourceItem(JASPListControl* _listControl, const QString& sourceName, const QString& sourceUse);

	SourceItem(JASPListControl* _listControl = nullptr);

	ListModel*				listModel()							{ return _listModel;				}
	const QString&			controlName()				const	{ return _controlName;				}
	const QStringList&		modelUse()					const	{ return _modelUse;					}
	bool					combineWithOtherModels()	const	{ return _combineWithOtherModels;	}
	const QSet<QString>&	usedControls()				const	{ return _usedControls;				}
	bool					isColumnsModel()			const	{ return _isColumnsModel;			}
	bool					isNativeModel()				const	{ return _nativeModel != nullptr;	}
	QAbstractItemModel*		nativeModel()						{ return _nativeModel;				}
	Terms					getTerms();

	static QVector<SourceItem*>				readAllSources(JASPListControl* _listControl);

private:
	static QString							_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse);
	static QString							_readRSourceName(const QString& sourceNameExt, QString& sourceUse);
	static QMap<QString, QVariant>			_readSource(JASPListControl* _listControl, const QVariant& source, JASPListControl::LabelValueMap& sourceValues, QVector<SourceItem*>& rSources, QAbstractItemModel*& _nativeModel);
	static JASPListControl::LabelValueMap	_readValues(JASPListControl* _listControl, const QVariant& _values);
	static SourceItem*						_readRSource(JASPListControl* listControl, const QVariant& rSource);
	static QList<QVariant>					_getListVariant(QVariant var);

	void									_setUp();
	Terms									_readAllTerms();

private slots:
	void									_connectModels();
	void									_resetModel();
	void									_dataChangedHandler(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles = QVector<int>());
	void									_rSourceChanged(const QString& name);

private:
	JASPListControl		*			_listControl			= nullptr;
	QString							_name,
									_controlName;
	QStringList						_modelUse;
	QVector<SourceItem*>			_discardSources;
	QVector<SourceItem*>			_rSources;
	JASPListControl::LabelValueMap	_values;
	bool							_isValuesSource			= false;
	bool							_isRSource				= false;
	ListModel			*			_listModel				= nullptr;
	QAbstractItemModel	*			_nativeModel			= nullptr;
	int								_nativeModelRole		= Qt::DisplayRole;
	bool							_isColumnsModel			= false;
	bool							_combineWithOtherModels	= false;
	QString							_conditionExpression;
	QVector<ConditionVariable>		_conditionVariables;
	QSet<QString>					_usedControls;
};

#endif // SOURCEITEM_H
