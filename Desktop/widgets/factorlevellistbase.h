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

#ifndef FACTORLEVELLISTBASE_H
#define FACTORLEVELLISTBASE_H

#include "jasplistcontrol.h"
#include "analysis/boundcontrolbase.h"
#include "listmodelfactorlevels.h"

class FactorLevelListBase :  public JASPListControl, public BoundControlBase
{
	Q_OBJECT
	
	Q_PROPERTY( QString			factorName			READ factorName				WRITE setFactorName				NOTIFY factorNameChanged			)
	Q_PROPERTY( QString			levelName			READ levelName				WRITE setLevelName				NOTIFY levelNameChanged				)
	Q_PROPERTY( QString			factorPlaceHolder	READ factorPlaceHolder		WRITE setFactorPlaceHolder		NOTIFY factorPlaceHolderChanged		)
	Q_PROPERTY( QString			levelPlaceHolder	READ levelPlaceHolder		WRITE setLevelPlaceHolder		NOTIFY levelPlaceHolderChanged		)
	Q_PROPERTY( int				minFactors			READ minFactors				WRITE setMinFactors				NOTIFY minFactorsChanged			)
	Q_PROPERTY( int				minLevels			READ minLevels				WRITE setMinLevels				NOTIFY minLevelsChanged				)

public:
	FactorLevelListBase(QQuickItem* parent = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	override;
	Json::Value		createJson()								override;
	void			bindTo(const Json::Value& value)			override;
	bool			encodeValue()						const	override	{ return true; }
	ListModel*		model()								const	override	{ return _factorLevelsModel; }
	void			setUpModel()								override;
	void			setUp()										override;

	QString			factorName()						const				{ return _factorName;			}
	QString			levelName()							const				{ return _levelName;			}
	QString			factorPlaceHolder()					const				{ return _factorPlaceHolder;	}
	QString			levelPlaceHolder()					const				{ return _levelPlaceHolder;		}
	int				minFactors()						const				{ return _minFactors;			}
	int				minLevels()							const				{ return _minLevels;			}

	QString			getFactorName(int i)				const				{ return QStringLiteral("%1 %2").arg(_factorName).arg(i);	}
	QString			getLevelName(int i)					const				{ return QStringLiteral("%1 %2").arg(_levelName).arg(i);	}

signals:
	void			itemChanged(int index, QString name);
	void			itemRemoved(int index);

	void			factorNameChanged();
	void			levelNameChanged();
	void			factorPlaceHolderChanged();
	void			levelPlaceHolderChanged();
	void			minFactorsChanged();
	void			minLevelsChanged();

protected slots:
	void			termsChangedHandler() override;

protected:
	GENERIC_SET_FUNCTION(FactorName,			_factorName,			factorNameChanged,			QString		)
	GENERIC_SET_FUNCTION(LevelName,				_levelName,				levelNameChanged,			QString		)
	GENERIC_SET_FUNCTION(FactorPlaceHolder,		_factorPlaceHolder,		factorPlaceHolderChanged,	QString		)
	GENERIC_SET_FUNCTION(LevelPlaceHolder,		_levelPlaceHolder,		levelPlaceHolderChanged,	QString		)
	GENERIC_SET_FUNCTION(MinFactors,			_minFactors,			minFactorsChanged,			int			)
	GENERIC_SET_FUNCTION(MinLevels,				_minLevels,				minLevelsChanged,			int			)

private:
	ListModelFactorLevels*				_factorLevelsModel	= nullptr;

	QString								_factorName			=	tr("Factor");
	QString								_levelName			=	tr("Level");
	QString								_factorPlaceHolder	=	tr("New Factor");
	QString								_levelPlaceHolder	=	tr("New Level");
	int									_minFactors			=	1,
										_minLevels			=	2;

};

#endif // FACTORLEVELLISTBASE_H
