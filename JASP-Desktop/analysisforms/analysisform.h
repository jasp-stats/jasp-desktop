//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef ANALYSISFORM_H
#define ANALYSISFORM_H

#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>
#include <QMap>

#include "dataset.h"
#include "options/options.h"
#include "options/optionvariables.h"

#include "availablefields.h"
#include "widgets/availablefieldslistview.h"
#include "widgets/assignbutton.h"
#include "widgets/boundlistview.h"

#include "widgets/tablemodelvariablesavailable.h"

#include "variableinfo.h"

class AnalysisForm : public QWidget, protected VariableInfoProvider
{
	Q_OBJECT

public:
	explicit AnalysisForm(QString name, QWidget *parent = 0);
	virtual void bindTo(Options *options, DataSet *dataSet);
	virtual void unbind();

	bool hasIllegalValue() const;
	const QString &illegalValueMessage() const;

signals:
	void illegalChanged();

protected:

	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const OVERRIDE;

	DataSet *_dataSet;
	Options *_options;

	TableModelVariablesAvailable _availableVariablesModel;

	OptionVariables *_mainVariables;

	void updateIllegalStatus();

	std::list<Bound *> _bounds;
	void illegalValueHandler(Bound *source);
	bool _hasIllegalValue;
	QString _illegalMessage;
	
	
};

#endif // ANALYSISFORM_H
