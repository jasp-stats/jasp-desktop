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

#ifndef REPEATEDMEASURESFACTORSLISTBASE_H
#define REPEATEDMEASURESFACTORSLISTBASE_H

#include "jasplistcontrol.h"
#include "analysis/boundcontrolbase.h"
#include "listmodelrepeatedmeasuresfactors.h"

class RepeatedMeasuresFactorsListBase :  public JASPListControl, public BoundControlBase
{
	Q_OBJECT
	
	Q_PROPERTY( QStringList		defaultFactors		READ defaultFactors			WRITE setDefaultFactors			NOTIFY defaultFactorsChanged		)
	Q_PROPERTY( QStringList		defaultLevels		READ defaultLevels			WRITE setDefaultLevels			NOTIFY defaultLevelsChanged			)
	Q_PROPERTY( QString			factorPlaceHolder	READ factorPlaceHolder		WRITE setFactorPlaceHolder		NOTIFY factorPlaceHolderChanged		)
	Q_PROPERTY( QString			levelPlaceHolder	READ levelPlaceHolder		WRITE setLevelPlaceHolder		NOTIFY levelPlaceHolderChanged		)

public:
	RepeatedMeasuresFactorsListBase(QQuickItem* parent = nullptr);

	bool			isJsonValid(const Json::Value& optionValue)	override;
	Json::Value		createJson()								override;
	void			bindTo(const Json::Value& value)			override;
	bool			encodeValue()						const	override	{ return true; }
	ListModel*		model()								const	override	{ return _factorsModel; }
	void			setUpModel()								override;
	void			setUp()										override;

	QStringList		defaultFactors()					const				{ return _defaultFactors;		}
	QStringList		defaultLevels()						const				{ return _defaultLevels;		}
	QString			factorPlaceHolder()					const				{ return _factorPlaceHolder;	}
	QString			levelPlaceHolder()					const				{ return _levelPlaceHolder;		}

signals:
	void			itemChanged(int index, QString name);
	void			itemRemoved(int index);

	void			defaultFactorsChanged();
	void			defaultLevelsChanged();
	void			factorPlaceHolderChanged();
	void			levelPlaceHolderChanged();

protected slots:
	void			termsChangedHandler() override;

protected:
	GENERIC_SET_FUNCTION(DefaultFactors,		_defaultFactors,		defaultFactorsChanged,		QStringList		)
	GENERIC_SET_FUNCTION(DefaultLevels,			_defaultLevels,			defaultLevelsChanged,		QStringList		)
	GENERIC_SET_FUNCTION(FactorPlaceHolder,		_factorPlaceHolder,		factorPlaceHolderChanged,	QString			)
	GENERIC_SET_FUNCTION(LevelPlaceHolder,		_levelPlaceHolder,		levelPlaceHolderChanged,	QString			)

private:
	ListModelRepeatedMeasuresFactors*	_factorsModel	= nullptr;

	QStringList							_defaultFactors		=	{ tr("RM Factor 1")					};
	QStringList							_defaultLevels		=	{ tr("Levels 1"), tr("Levels 2")	};
	QString								_factorPlaceHolder	=	tr("New Factor");
	QString								_levelPlaceHolder	=	tr("New Level");

};

#endif // REPEATEDMEASURESFACTORSLISTBASE_H
