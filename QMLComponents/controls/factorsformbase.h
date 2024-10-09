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

#ifndef FACTORSFORMBASE_H
#define FACTORSFORMBASE_H

#include "jasplistcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include "models/listmodelfactorsform.h"


class FactorsFormBase :  public JASPListControl, public BoundControlBase
{
	Q_OBJECT

	Q_PROPERTY( int		initNumberFactors		READ initNumberFactors		WRITE setInitNumberFactors	NOTIFY initNumberFactorsChanged	)
	Q_PROPERTY( int		countVariables			READ countVariables										NOTIFY countVariablesChanged	)
	Q_PROPERTY(QString	baseName				READ baseName				WRITE setBaseName			NOTIFY baseNameChanged			)
	Q_PROPERTY(QString	baseTitle				READ baseTitle				WRITE setBaseTitle			NOTIFY baseTitleChanged			)
	Q_PROPERTY(int		startIndex				READ startIndex				WRITE setStartIndex			NOTIFY startIndexChanged		)
	Q_PROPERTY(bool		nested					READ nested					WRITE setNested				NOTIFY nestedChanged			)
	Q_PROPERTY(bool		allowInteraction		READ allowInteraction		WRITE setAllowInteraction	NOTIFY allowInteractionChanged	)

public:
	FactorsFormBase(QQuickItem* parent = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	const	override;
	Json::Value		createJson()								const	override;
	void			bindTo(const Json::Value& value)					override;
	ListModel*		model()										const	override	{ return _factorsModel; }
	void			setUpModel()										override;

	Q_INVOKABLE	void	addFactor()													{ _factorsModel->addFactor();						}
	Q_INVOKABLE void	removeFactor()												{ _factorsModel->removeFactor();					}
	Q_INVOKABLE void	titleChanged(int index, QString title)						{ _factorsModel->titleChangedSlot(index, title);	}
	Q_INVOKABLE void	factorAdded(int index, QVariant item);

	int					initNumberFactors()						const				{ return _initNumberFactors;						}
	int					countVariables()						const				{ return _initialized ? _factorsModel->countVariables() : 0; }
	JASPListControl*	availableVariablesList()				const				{ return _availableVariablesListItem;				}
	QString				baseName()								const				{ return _baseName;		}
	QString				baseTitle()								const				{ return _baseTitle;	}
	int					startIndex()							const				{ return _startIndex;	}
	bool				nested()								const				{ return _nested;		}
	bool				allowInteraction()						const				{ return _allowInteraction;		}

	GENERIC_SET_FUNCTION(BaseName			, _baseName			, baseNameChanged			, QString		)
	GENERIC_SET_FUNCTION(BaseTitle			, _baseTitle		, baseTitleChanged			, QString		)
	GENERIC_SET_FUNCTION(StartIndex			, _startIndex		, startIndexChanged			, int			)
	GENERIC_SET_FUNCTION(Nested				, _nested			, nestedChanged				, bool			)
	GENERIC_SET_FUNCTION(AllowInteraction	, _allowInteraction	, allowInteractionChanged	, bool			)

signals:
	void initNumberFactorsChanged();
	void countVariablesChanged();
	void baseNameChanged();
	void baseTitleChanged();
	void startIndexChanged();
	void nestedChanged();
	void allowInteractionChanged();

protected slots:
	void			termsChangedHandler() override;

protected:
	GENERIC_SET_FUNCTION(InitNumberFactors,	_initNumberFactors,		initNumberFactorsChanged,	int	)

private:
	ListModelFactorsForm*	_factorsModel				= nullptr;
	JASPListControl*		_availableVariablesListItem	= nullptr;
	int						_initNumberFactors			= 1,
							_startIndex					= 1;
	QString					_availableVariablesListName,
							_baseName					= "Factor",   // dont translate it, see: https://github.com/jasp-stats/jasp-issues/issues/2947
							_baseTitle					= tr("Factor");
	bool					_nested						= false,
							_allowInteraction			= false;

};

#endif // FACTORSFORMBASE_H
