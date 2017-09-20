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

#ifndef TABLEMODELANOVAMODEL_H
#define TABLEMODELANOVAMODEL_H

#include <QAbstractListModel>
#include <list>

#include "tablemodelvariablesavailable.h"
#include "enhanceddroptarget.h"
#include "options/optionterm.h"
#include "options/optionstable.h"

#include "tablemodel.h"

class TableModelAnovaModel : public TableModel, public EnhancedDropTarget, public BoundModel
{
	Q_OBJECT

	friend class AnovaModelWidget;

	enum AssignType { Cross = 0, MainEffects, Interaction, All2Way, All3Way, All4Way, All5Way };

public:
	TableModelAnovaModel(QObject *parent = 0);

	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) OVERRIDE;

	virtual QStringList mimeTypes() const OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent = QModelIndex()) OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int assignType) OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;

	const Terms &variables() const;

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

	const Terms &terms() const;

	bool piecesCanBeAssigned() const;
	void setPiecesCanBeAssigned(bool piecesCanBeAssigned);

public slots:

	void setVariables(const Terms &fixedFactors, const Terms &randomFactors = Terms(), const Terms &covariates = Terms());

	void addFixedFactors(const Terms &terms);
	void addRandomFactors(const Terms &terms);
	void addCovariates(const Terms &terms);
	void removeVariables(const Terms &terms);

signals:
	void variablesAvailableChanged();
	void termsChanged();

protected:

	static OptionTerm* termOptionFromRow(Options *row);

	void setTerms(const Terms &terms, bool newTermsAreNuisance = false);

	void clear();
	void assign(const Terms &terms);
	void updateNuisances(bool checked = true);

	OptionsTable *_boundTo;

	std::vector<Options *> _rows;

	bool _piecesCanBeAssigned;

	Terms _variables;

	Terms _covariates;
	Terms _fixedFactors;
	Terms _randomFactors;

	Terms _terms;

};

#endif // TABLEMODELANOVAMODEL_H
