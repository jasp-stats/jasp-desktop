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


#ifndef BOUNDQMLTEXTAREA_H
#define BOUNDQMLTEXTAREA_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optionstring.h"
#include "lavaansyntaxhighlighter.h"
#include "listmodeltermsavailable.h"
#include "qmllistview.h"

#include <QObject>

#include <QString>
#include <QRegularExpression>
#include <QList>
#include <QDebug>

class BoundQMLTextArea : public QMLListView, public BoundQMLItem
{
	Q_OBJECT
	
	enum TextType {Default, Model, Rcode, JAGSmodel, Source};
	
public:
	BoundQMLTextArea(JASPControlBase* item);

	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;
	Option*		boundTo()									override { return _boundTo; }
	ListModel*	model()								const	override { return _model; }

	void		resetQMLItem(JASPControlBase *item)			override;
	void		rScriptDoneHandler(const QString &result)	override;

public slots:
	virtual	void checkSyntax();
			void dataSetChangedHandler();
    
protected:
	void						_setSourceTerms();

	OptionString*				_boundTo = nullptr;
	QString						_text;
	TextType					_textType;
	QList<QString>				_separators;
	
	LavaanSyntaxHighlighter*	_lavaanHighlighter = nullptr;
	ListModelTermsAvailable*	_model = nullptr;
	
};


#endif // BOUNDQMLTEXTAREA_H
