#include "csvpreviewmodel.h"
#include "utilities/desktopcommunicator.h"
#include "utilities/qutils.h"

CsvPreviewModel::CsvPreviewModel(QObject *parent) : QAbstractTableModel(parent)
{
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
	emit delimiterChanged();
	updateInternalStructure();
}

void CsvPreviewModel::setDelimiterFromChar(char delim)
{
	setDelimiter(QChar(delim));
}

void CsvPreviewModel::preparePreview(const QString &data, char delimiter)
{
	setRawData(data);
	setDelimiter(QChar(delimiter));
	setVisible(true);
}

void CsvPreviewModel::updateLocale()
{
	updateInternalStructure();
}

void CsvPreviewModel::updateInternalStructure()
{
	// Prepare the model for a complete reset
	beginResetModel();

	_grid.clear();
	if (_rawData.isEmpty()) {
		endResetModel();
		return;
	}

	// Split data into rows (assuming newlines separate rows)
	QStringList rows = _rawData.split('\n', Qt::SkipEmptyParts);
	
	for (const QString &rowString : rows) {
		// Split each row by the chosen delimiter
		QStringList columns = rowString.split(_delimiter);
		_grid.append(columns);
	}

	endResetModel();
	
	clearTableForResize();
}

int CsvPreviewModel::rowCount(const QModelIndex &) const
{
	return _grid.count();
}

int CsvPreviewModel::columnCount(const QModelIndex &) const
{
	if (_grid.isEmpty()) 
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
		QString val = _grid[r][c];

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
