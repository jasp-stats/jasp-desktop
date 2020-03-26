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
									modelUse;
		ListModel	*				model;
		QVector<SourceType>			discardModels;
		QString						conditionExpression;
		QVector<ConditionVariable>	conditionVariables;

		SourceType(
				  const QString& _name = ""
				, const QString& _modelUse = ""
				, const QVector<QPair<QString, QString> >& _discardModels = QVector<QPair<QString, QString> >()
				, const QString& _conditionExpression = ""
				, const QVector<QMap<QString, QVariant> >& _conditionVariables = QVector<QMap<QString, QVariant> >())
			: name(_name), modelUse(_modelUse), model(nullptr), conditionExpression(_conditionExpression)
		{
			for (const QPair<QString, QString>& discardModel : _discardModels)
				discardModels.push_back(SourceType(discardModel.first, discardModel.second));

			for (const QMap<QString, QVariant>& conditionVariable : _conditionVariables)
			{
				conditionVariables.push_back(ConditionVariable(conditionVariable["name"].toString()
											, conditionVariable["component"].toString()
											, conditionVariable["property"].toString()
											, conditionVariable["addQuotes"].toBool())
							);
			}
		}
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

			JASPControlWrapper*	getRowControl(const QString& key, const QString& name)		const;
			bool				addRowControl(const QString& key, JASPControlWrapper* control);

			JASPControlWrapper*	getChildControl(QString key, QString name) override;

	Q_INVOKABLE QString			getSourceType(QString name);

			QMLListView::SourceType* getSourceTypeFromModel(ListModel* model);

protected slots:
	virtual void				modelChangedHandler() {} // This slot must be overriden in order to update the options when the model has changed
			void				sourceChangedHandler();

protected:
	virtual void				setSources();
	virtual void				readModelProperty(QMap<QString, QString>* map = nullptr);
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
	int		_getAllowedColumnsTypes();
	void	_setAllowedVariables();


	QList<QVariant> _getListVariant(QVariant var);
};

#endif // QMLLISTVIEW_H
