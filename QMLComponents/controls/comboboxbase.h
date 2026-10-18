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

#ifndef COMBOBOXBASE_H
#define COMBOBOXBASE_H

#include "jasplistcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include "models/listmodel.h"
#include "models/listmodeltermsavailable.h"
#include <QMap>

class ComboBoxBase : public JASPListControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( int					currentIndex				READ currentIndex				WRITE setCurrentIndex			NOTIFY currentIndexChanged			)
	Q_PROPERTY( QString				currentText					READ currentLabel				WRITE setCurrentLabel			NOTIFY currentLabelChanged			)
	Q_PROPERTY( QString				currentValue				READ currentValue				WRITE setCurrentValue			NOTIFY currentValueChanged			)
	Q_PROPERTY( QString				startValue					READ startValue					WRITE setStartValue				NOTIFY startValueChanged			)
	Q_PROPERTY( QString				currentColumnType			READ currentColumnType											NOTIFY currentColumnTypeChanged		)
	Q_PROPERTY( QString				currentColumnTypeIcon		READ currentColumnTypeIcon										NOTIFY currentColumnTypeIconChanged	)
	Q_PROPERTY( QString				longestValue				READ longestValue												NOTIFY longestValueChanged			)
	///< True while this combo is bound to a value its model cannot serve yet: the combo stays
	///blank, the option keeps the saved value, and it binds for real as soon as a model update
	///contains it (see bindTo). FilterSelect uses it to not pull the dropdown to its host over
	///top of a value that is still waiting.
	Q_PROPERTY( bool				valueAwaitsModel			READ valueAwaitsModel			NOTIFY currentValueChanged			)

public:
	ComboBoxBase(QQuickItem* parent = nullptr);

	void				bindTo(const Json::Value& value)			override;
	bool				isJsonValid(const Json::Value& optionValue) const	override;
	Json::Value			createJson()								const	override;
	void				setUp()												override;
	ListModel*			model()										const	override	{ return _model;				}
	void				setUpModel()										override;
	QString				generateMDHelp(int depth = 0)				const	override;
	QString				generateDoxygenHelp()						const	override;
	bool				hasInfoSomewhere()									const	override;
	void				setBoundValue(const Json::Value &value, bool emitChanges = true)	override;

	const QString&		currentLabel()								const				{ return _currentLabel;			}
	const QString&		currentValue()								const				{ return _currentValue;			}
	const QString&		startValue()								const				{ return _startValue;			}
	const QString&		currentColumnType()							const				{ return _currentColumnType;	}
	const QString&		currentColumnTypeIcon()						const				{ return _currentColumnTypeIcon;}
	int					currentIndex()								const				{ return _currentIndex;			}
	QString				longestValue()								const				{ return _longestLabel;			}
	bool				valueAwaitsModel()							const				{ return _valueAwaitsModel;		}

protected:
	///< mds-plan D3.4: while a bound value awaits its model (valueAwaitsModel) the generated
	/// Engine value is the control's OWN held raw value (C2/BIND: the option keeps exactly what
	/// came in, never a coerced/blank model projection) - the same value the deleted
	/// analysis-wide mirror stored for a pending combo. Bound normally, the model-derived
	/// createJson() equals the held value, so this override only matters in the pending state.
	Json::Value			createEngineValue()							const	override;

public:

	stringvec			usedVariables()								const	override;
	bool				encodeValue()								const	override	{ return containsVariables();	}

	///< mds-plan D3.2: scalar option-level sidecar for the single selected variable: the selected
	/// model term's filterId when qualified (the model is the typed state), else the host's id
	/// (the option-level sidecar id of the aware world), null = unqualified. See BoundControlTerms::createFilterIds.
	Json::Value			createFilterIds()															const	override;


signals:
	void currentLabelChanged();
	void currentValueChanged();
	void startValueChanged();
	void currentColumnTypeChanged();
	void currentColumnTypeIconChanged();
	void currentIndexChanged();
	void activated(int index);
	void longestValueChanged();

protected slots:
	void termsChangedHandler()				override;
	void setCurrentIndex(int index);
	void setCurrentValue(QString value);
	void setCurrentLabel(QString label);
	void activatedSlot(int index);

	GENERIC_SET_FUNCTION(StartValue,	_startValue,	startValueChanged,	QString	)

protected:
	bool _checkLevelsConstraints()			override;


	ListModelTermsAvailable*	_model					= nullptr;
	QString						_currentLabel,
								_currentValue,
								_startValue,
								_currentColumnType,
								_currentColumnRealType,
								_currentColumnTypeIcon;
	std::string					_unusedInitialValue;
	///< A combo bound to a value its model cannot serve yet keeps it pending in
	///_unusedInitialValue and sets this flag: bindTo/termsChangedHandler wait instead of
	///coercing to entry 0, until the model catches up or a user/host choice supersedes it.
	bool						_valueAwaitsModel	= false;
	int							_currentIndex			= -1;
	QString						_longestLabel;

	int	 _getStartIndex()											const;
	void _resetItemWidth();
	void _setCurrentProperties(int index, bool bindValue = true);
	bool _hasOptionInfo()											const;
	std::string _findType(std::string value)						const;

};

#endif // COMBOBOXBASE_H
