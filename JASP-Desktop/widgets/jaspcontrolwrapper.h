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

#ifndef JASPCONTROLWRAPPER_H
#define JASPCONTROLWRAPPER_H

#include <QString>
#include <set>
#include <QVariant>

class JASPControlBase;
class AnalysisForm;

class JASPControlWrapper
{

public:
	typedef std::set<JASPControlWrapper*> Set;

			 JASPControlWrapper(JASPControlBase* item);
	virtual ~JASPControlWrapper() {}

	virtual void				setUp();
	virtual void				cleanUp();
	virtual void				resetQMLItem(JASPControlBase* item);

	const QString			&	name()					const;
	bool						isBound()				const;
	AnalysisForm			*	form()					const;
	JASPControlBase			*	item()					const	{ return _item; }
	const Set				&	depends()				const	{ return _depends; }
	void						addControlError(const QString& error);
	bool						addDependency(JASPControlWrapper* item);
	void						removeDependency(JASPControlWrapper* item);
	QVariant					getItemProperty(const QString& name);
	void						setItemProperty(const QString& name, const QVariant& value);
	JASPControlWrapper		*	parentListControl();
	QString						parentListControlKey();
	virtual JASPControlWrapper*	getChildControl(QString key, QString name)	{ return nullptr; }


	static JASPControlWrapper*	buildJASPControlWrapper(JASPControlBase* control);

	virtual QString				friendlyName() const;
	
protected:
	
	JASPControlBase	*	_item = nullptr;
	Set					_depends; //So Joris changed this to a set instead of a vector because that is what it seemed to be, the order isn't important right?
};

#endif // JASPCONTROLWRAPPER_H
