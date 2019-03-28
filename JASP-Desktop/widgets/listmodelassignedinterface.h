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

#ifndef LISTMODELASSIGNEDINTERFACE_H
#define LISTMODELASSIGNEDINTERFACE_H

#include "listmodeldraggable.h"
#include "listmodelavailableinterface.h"

class ListModelExtraControls;
class Options;

class ListModelAssignedInterface : public ListModelDraggable
{
	Q_OBJECT
public:
	ListModelAssignedInterface(QMLListView* listView);
	
	QVariant		data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
	void			endResetModel() override;
		
	virtual void	setAvailableModel(ListModelAvailableInterface *source);
	ListModelAvailableInterface* source() const											{ return _source; }
	void addExtraControls(const QVector<QMap<QString, QVariant> >& extraControlColumns);
	ListModelExtraControls* getExtraControlModel(QString colName)						{ return _extraControlsModels[colName]; }
	
public slots:
	virtual void availableTermsChanged(Terms *termsAdded, Terms *termsRemoved) {}

signals:
	void extraControlsChanged();

protected:
	void addExtraControls();
	
	ListModelAvailableInterface*			_source;
	QVector<QMap<QString, QVariant> >		_extraControlsDefinitions;
	QMap<QString, bool>						_extraControlsNames;
	QMap<QString, ListModelExtraControls* > _extraControlsModels;
	QMap<int, QString>						_rowNames;
	QMap<QString, ListModelExtraControls* > _modelCache;
};

#endif // LISTMODELASSIGNEDINTERFACE_H
