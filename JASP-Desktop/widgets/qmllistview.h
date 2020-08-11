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

#ifndef QMLLISTVIEW_H
#define QMLLISTVIEW_H

#include "jaspcontrolwrapper.h"
#include "common.h"
#include <QObject>
#include <QVector>
#include <QSet>

class ListModel;
class BoundQMLItem;
class Options;
class RowControls;

class QMLListView : public QObject, public virtual JASPControlWrapper
{
Q_OBJECT

public:
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
		ListModel	*				model;
		QVector<SourceType>			discardModels;
		QString						conditionExpression;
		QVector<ConditionVariable>	conditionVariables;
		bool						combineWithOtherModels = false;
		QSet<QString>				usedControls;

		SourceType(
				  const QString& _name = ""
				, const QString& _controlName = ""
				, const QString& _modelUse = ""
				, const QVector<std::tuple<QString, QString, QString> >& _discardModels = QVector<std::tuple<QString, QString, QString> >()
				, const QString& _conditionExpression = ""
				, const QVector<QMap<QString, QVariant> >& _conditionVariables = QVector<QMap<QString, QVariant> >()
				, bool _combineWithOtherModels = false);

		QVector<SourceType> getDiscardModels(bool onlyNotNullModel = true)	const;
	};
	
	QMLListView(JASPControlBase* item);
	
	virtual ListModel		*	model()			const = 0;
			void				setUp()			override;
			void				cleanUp()		override;
	
	virtual void				setTermsAreNotVariables();
	virtual void				setTermsAreInteractions();
	
			int					variableTypesAllowed()		const	{ return _variableTypesAllowed; }

	const QList<SourceType*>&	sourceModels()				const	{ return _sourceModels; }
			bool				hasSource()					const	{ return _hasSource; }
			bool				modelHasAllVariables()		const	{ return _modelHasAllVariables; }
			void				setModelHasAllVariables(bool b)		{ _modelHasAllVariables = b; }

			JASPControlWrapper*	getRowControl(const QString& key, const QString& name)		const;
			bool				addRowControl(const QString& key, JASPControlWrapper* control);

			JASPControlWrapper*	getChildControl(QString key, QString name) override;

	Q_INVOKABLE QString			getSourceType(QString name);

protected slots:
	virtual void				modelChangedHandler() {} // This slot must be overriden in order to update the options when the model has changed
			void				sourceChangedHandler();

protected:
	virtual void				setSources();
			void				addRowComponentsDefaultOptions(Options* optionTable);

protected:
	QList<SourceType*>	_sourceModels;
	bool				_hasSource				= false;
	bool				_needsSourceModels;
	int					_variableTypesAllowed;
	bool				_hasRowComponents		= false;
	std::string			_optionKeyName;
	bool				_modelHasAllVariables	= false;
	RowControls*		_defaultRowControls		= nullptr;

	static const QString _defaultKey;
	
private:
	int						_getAllowedColumnsTypes();
	void					_setAllowedVariables();
	QString					_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse);
	QMap<QString, QVariant> _readSource(const QVariant& source, QString& sourceName, QString& sourceControl, QString& sourceUse);




	QList<QVariant> _getListVariant(QVariant var);
};

#endif // QMLLISTVIEW_H
