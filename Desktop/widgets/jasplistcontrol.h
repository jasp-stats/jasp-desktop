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
#include "../analysis/boundcontrol.h"
#include "common.h"
#include <QObject>
#include <QVector>
#include <QMap>
#include <QSet>
#include <QAbstractItemModel>

class ListModel;
class Options;
class RowControls;
class Terms;
class SourceItem;

class JASPListControl : public JASPControl
{
	Q_OBJECT

	Q_PROPERTY( ListModel*		model					READ model													NOTIFY modelChanged					)
	Q_PROPERTY( QVariant		source					READ source					WRITE setSource					NOTIFY sourceChanged				)
	Q_PROPERTY( QVariant		values					READ values					WRITE setValues					NOTIFY sourceChanged				)
	Q_PROPERTY( int				count					READ count													NOTIFY countChanged					)
	Q_PROPERTY( QString			optionKey				READ optionKey				WRITE setOptionKey													)
	Q_PROPERTY( bool			addEmptyValue			READ addEmptyValue			WRITE setAddEmptyValue			NOTIFY addEmptyValueChanged			)
	Q_PROPERTY( QString			placeholderText			READ placeholderText		WRITE setPlaceHolderText		NOTIFY placeHolderTextChanged		)
	Q_PROPERTY( QString			labelRole				READ labelRole				WRITE setLabelRole				NOTIFY labelRoleChanged				)
	Q_PROPERTY( QString			valueRole				READ valueRole				WRITE setValueRole				NOTIFY valueRoleChanged				)
	Q_PROPERTY( bool			containsVariables		READ containsVariables										NOTIFY containsVariablesChanged		)
	Q_PROPERTY( bool			containsInteractions	READ containsInteractions									NOTIFY containsInteractionsChanged	)


public:
	typedef QVector<std::pair<QString, QString> > LabelValueMap;

	JASPListControl(QQuickItem* parent);
	
	virtual ListModel		*	model()						const	= 0;
	virtual void				setUpModel();
			void				setUp()						override;
			void				cleanUp()					override;
	
			int					variableTypesAllowed()		const			{ return _variableTypesAllowed; }

	const QVector<SourceItem*>& sourceItems()				const			{ return _sourceItems; }
			void				applyToAllSources(std::function<void(SourceItem *sourceItem, const Terms& terms)> applyThis);

			bool				hasSource()					const			{ return _sourceItems.size() > 0; }

			JASPControl		*	getRowControl(const QString& key, const QString& name)	const;
	virtual	bool				addRowControl(const QString& key, JASPControl* control);
			bool				hasRowComponent()			const;

			const QString&		optionKey()					const			{ return _optionKey; }
			JASPControl		*	getChildControl(QString key, QString name) override;

	Q_INVOKABLE QString			getSourceType(QString name);

			const QVariant&		source()					const			{ return _source;				}
			const QVariant&		values()					const			{ return _values;				}
			int					count();
			bool				addEmptyValue()				const			{ return _addEmptyValue;		}
			const QString&		placeholderText()			const			{ return _placeHolderText;		}
			const QString&		labelRole()					const			{ return _labelRole;			}
			const QString&		valueRole()					const			{ return _valueRole;			}
			bool				containsVariables()			const			{ return _containsVariables;	}
			bool				containsInteractions()		const			{ return _containsInteractions;	}
			bool				encodeValue()				const override	{ return containsVariables();	}
			bool				useSourceLevels()			const			{ return _useSourceLevels;		}
			void				setUseSourceLevels(bool b)					{ _useSourceLevels = b;			}

signals:
			void				modelChanged();
			void				sourceChanged();
			void				countChanged();
			void				addEmptyValueChanged();
			void				placeHolderTextChanged();
			void				labelRoleChanged();
			void				valueRoleChanged();
			void				containsVariablesChanged();
			void				containsInteractionsChanged();

public slots:
			void				setContainsVariables();
			void				setContainsInteractions();

protected slots:
	virtual void				termsChangedHandler() {} // This slot must be overriden in order to update the options when the model has changed
			void				sourceChangedHandler();

			void				setOptionKey(const QString& optionKey)	{ _optionKey = optionKey; }

			GENERIC_SET_FUNCTION(Source,				_source,				sourceChanged,					QVariant	)
			GENERIC_SET_FUNCTION(Values,				_values,				sourceChanged,					QVariant	)
			GENERIC_SET_FUNCTION(AddEmptyValue,			_addEmptyValue,			addEmptyValueChanged,			bool		)
			GENERIC_SET_FUNCTION(PlaceHolderText,		_placeHolderText,		placeHolderTextChanged,			QString		)
			GENERIC_SET_FUNCTION(LabelRole,				_labelRole,				labelRoleChanged,				QString		)
			GENERIC_SET_FUNCTION(ValueRole,				_valueRole,				valueRoleChanged,				QString		)

protected:
	QVector<SourceItem*>	_sourceItems;
	int						_variableTypesAllowed	= 0xff;
	QString					_optionKey				= "value";
	RowControls*			_defaultRowControls		= nullptr;
	QVariant				_source;
	QVariant				_values;
	bool					_addEmptyValue			= false,
							_containsVariables		= false,
							_containsInteractions	= false,
							_termsAreInteractions	= false,
							_useSourceLevels		= false;
	QString					_placeHolderText		= tr("<no choice>"),
							_labelRole				= "label",
							_valueRole				= "value";

private:
	void									_setupSources();
	Terms									_getCombinedTerms(SourceItem* sourceToCombine);
};

#endif // JASPLISTCONTROL_H
