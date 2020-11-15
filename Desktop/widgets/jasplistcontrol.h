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

#ifndef JASPLISTCONTROL_H
#define JASPLISTCONTROL_H

#include "../analysis/jaspcontrol.h"
#include "common.h"
#include <QObject>
#include <QVector>
#include <QMap>
#include <QSet>

class ListModel;
class Options;
class RowControls;
class Terms;

class JASPListControl : public JASPControl
{
Q_OBJECT

public:
	typedef QMap<QString, QString> LabelValueMap;
	struct SourceType
	{
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

		QString						name,
									controlName,
									modelUse;
		QVector<SourceType>			discardModels;
		LabelValueMap				values;
		bool						isValuesSource = false;
		ListModel	*				model = nullptr;
		QString						conditionExpression;
		QVector<ConditionVariable>	conditionVariables;
		bool						combineWithOtherModels = false;
		QSet<QString>				usedControls;

		SourceType(
				  const QString& _name
				, const QString& _controlName
				, const QString& _modelUse
				, const LabelValueMap& _values
				, bool isValuesSource
				, const QVector<std::tuple<QString, QString, QString, LabelValueMap, bool> >& _discardModels = QVector<std::tuple<QString, QString, QString, LabelValueMap, bool> >()
				, const QString& _conditionExpression = ""
				, const QVector<QMap<QString, QVariant> >& _conditionVariables = QVector<QMap<QString, QVariant> >()
				, bool _combineWithOtherModels = false);

		SourceType(const LabelValueMap& _values) : values(_values), isValuesSource(true) {}

		QVector<SourceType> getDiscardModels(bool onlyNotNullModel = true)	const;
	};
	
	JASPListControl(QQuickItem* parent);
	
	virtual ListModel		*	model()			const	= 0;
	virtual void				setUpModel()			= 0;
			void				setUp()			override;
			void				cleanUp()		override;
	
			int					variableTypesAllowed()		const	{ return _variableTypesAllowed; }

	const QList<SourceType*>&	sourceModels()				const	{ return _sourceModels; }
	QList<std::pair<SourceType*, Terms> >	getTermsPerSource();
			bool				hasSource()					const	{ return _hasSource; }

			JASPControl		*	getRowControl(const QString& key, const QString& name)		const;
			bool				addRowControl(const QString& key, JASPControl* control);
			bool				hasRowComponent()			const;

			void				addRowComponentsDefaultOptions(Options* optionTable);
			const std::string&	optionKeyName()				const	{ return _optionKeyName; }
			JASPControl		*	getChildControl(QString key, QString name) override;

	Q_INVOKABLE QString			getSourceType(QString name);

protected slots:
	virtual void				modelChangedHandler() {} // This slot must be overriden in order to update the options when the model has changed
			void				sourceChangedHandler();

protected:
	virtual void				setupSources();

protected:
	QList<SourceType*>	_sourceModels;
	bool				_hasSource				= false;
	bool				_needsSourceModels		= false;
	int					_variableTypesAllowed;
	std::string			_optionKeyName;
	RowControls*		_defaultRowControls		= nullptr;

	static const QString _defaultKey;
	
private:
	int						_getAllowedColumnsTypes();
	void					_setAllowedVariables();
	QString					_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse);
	QMap<QString, QVariant>	_readSource(const QVariant& source, QString& sourceName, QString& sourceControl, QString& sourceUse, LabelValueMap& sourceValues);
	LabelValueMap			_readValues(const QVariant& values);
	QList<QVariant>			_getListVariant(QVariant var);
};

#endif // JASPLISTCONTROL_H
