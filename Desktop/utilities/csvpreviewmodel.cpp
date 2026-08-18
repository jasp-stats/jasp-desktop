//
// Copyright (C) 2026 University of Amsterdam
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
#include "csvpreviewmodel.h"
#include "utilities/desktopcommunicator.h"
#include "utilities/qutils.h"

CsvPreviewModel::CsvPreviewModel(QObject *parent) : QAbstractTableModel(parent)
{
	_parser = new CSVParser(',', true);
}

CsvPreviewModel::~CsvPreviewModel()
{
	// CSVParser is a QObject with parent-child relationship, automatic cleanup
}

void CsvPreviewModel::setRawData(const QString &data)
{
	if (_rawData == data) return;
	_rawData = data;
	emit rawDataChanged();
	updateInternalStructure();
}

void CsvPreviewModel::setDelimiter(QChar delim)
{
	if (_delimiter == delim) return;
	_delimiter = delim;
	_parser->setDelimiter(delim.toLatin1());
	emit delimiterChanged();
	updateInternalStructure();
}

void CsvPreviewModel::setDelimiterFromChar(char delim)
{
	setDelimiter(QChar(delim));
}

void CsvPreviewModel::preparePreview(const QString &data, char delimiter)
{
	setDelimiter(QChar(delimiter));
	setRawData(data);
	setVisible(true);
}

void CsvPreviewModel::updateLocale()
{
	updateInternalStructure();
}

void CsvPreviewModel::updateInternalStructure()
{
	beginResetModel();

	if (_rawData.isEmpty()) 
	{
		_grid.clear();
		endResetModel();
		return;
	}

	// Parse using CSVParser
	_grid = _parser->parse(_rawData.toStdString());

	endResetModel();
	emit clearTableForResize();
}

int CsvPreviewModel::rowCount(const QModelIndex &) const
{
	return _grid.size();
}

int CsvPreviewModel::columnCount(const QModelIndex &) const
{
	if (_grid.empty()) 
		return 0;
	
	// Find the max number of columns across all rows to ensure a rectangular grid
	int maxCols = 0;
	for (const auto &row : _grid)
		if (row.size() > maxCols) 
			maxCols = row.size();
	
	return maxCols;
}

QVariant CsvPreviewModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid() || role != Qt::DisplayRole)
		return QVariant();

	int r = index.row();
	int c = index.column();

	// Check if the row exists and if this row has a column at this index
	if (r < _grid.size() && c < _grid[r].size()) {
		QString val = QString::fromStdString(_grid[r][c]);

		if (val.isEmpty()) {
			if (r == 0)
				return QVariant(QString("V") + QString::number(c + 1));
			return QVariant();
		}

		if (r == 0) // Do not change the column names
			return val;

		double dblVal;
		if (QColumnUtils::getDoubleValue(val, dblVal, true))
			return QVariant(QColumnUtils::doubleToString(dblVal));

		// Add quotes to signify that this will be considered as a string
		return QVariant("\"" + val + "\"");
	}

	return QVariant();
}

QHash<int, QByteArray> CsvPreviewModel::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[Qt::DisplayRole] = "display";
	return roles;
}

bool CsvPreviewModel::visible() const
{
	return _visible;
}

void CsvPreviewModel::setVisible(bool newVisible)
{
	if (_visible == newVisible)
		return;
	
	_visible = newVisible;
	emit visibleChanged();
	
	if(!_visible)
		DesktopCommunicator::singleton()->delimiterChosen(_delimiter.toLatin1());
}


