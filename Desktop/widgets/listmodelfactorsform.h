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

#ifndef LISTMODELFACTORSFORM_H
#define LISTMODELFACTORSFORM_H

#include "listmodel.h"

class JASPListControl;
class VariablesListBase;

class ListModelFactorsForm : public ListModel
{
	Q_OBJECT
	Q_PROPERTY(int count	READ count)
	
public:
	enum ListModelFactorsFormRoles {
        FactorNameRole = Qt::UserRole + 1,
		FactorTitleRole
    };
	typedef std::vector<std::tuple<std::string, std::string, std::vector<std::string> > > FactorVec;

	ListModelFactorsForm(JASPListControl* listView);
	
	QHash<int, QByteArray>	roleNames()													const override;
	int						rowCount(const QModelIndex &parent = QModelIndex())			const override { return count(); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	
	Terms					termsEx(const QString& what)									override;
	void					initFactors(const FactorVec &factors);
	int						count() const { return int(_factors.size()); }
	FactorVec				getFactors();
	
	void					addFactor();
	void					removeFactor();
	void					factorAdded(int, VariablesListBase* listView);

public slots:
	void titleChangedSlot(int index, QString title);
	void resetModelTerms();

signals:
	void addListView(JASPListControl* listView);
	
protected:
	struct Factor
	{
		QString						name;
		QString						title;
		JASPListControl*			listView;
		std::vector<std::string>	initTerms;
		Factor(const QString& _name, const QString& _title, std::vector<std::string> _initTerms = std::vector<std::string>()) :
			name(_name), title(_title), listView(nullptr), initTerms(_initTerms) {}
	};
	QVector<Factor*>	_factors;
	
};

#endif // LISTMODELFACTORSFORM_H
