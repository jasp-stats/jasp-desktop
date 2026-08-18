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
#ifndef CSVPREVIEWMODEL_H
#define CSVPREVIEWMODEL_H

#include <QAbstractTableModel>
#include <QStringList>
#include <QChar>
#include "data/importers/csv/csvparser.h"

class CsvPreviewModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(QString	rawData		READ rawData	WRITE setRawData	NOTIFY rawDataChanged)
	Q_PROPERTY(QChar	delimiter	READ delimiter	WRITE setDelimiter	NOTIFY delimiterChanged)
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged)

public:
	explicit CsvPreviewModel(QObject *parent = nullptr);
	~CsvPreviewModel();

	int						rowCount(	const QModelIndex &parent = QModelIndex())				const override;
	int						columnCount(const QModelIndex &parent = QModelIndex())				const override;
	QVariant				data(		const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()															const override;

	QString					rawData() const { return _rawData; }
	void					setRawData(const QString &data);

	QChar					delimiter() const { return _delimiter; }
	void					setDelimiter(QChar delim);
	void					setDelimiterFromChar(char delim);
	void					preparePreview(const QString &data, char delimiter);

	bool					visible() const;
	void					setVisible(bool newVisible);
	
public slots:
	void					updateLocale();

signals:
	void					rawDataChanged();
	void					delimiterChanged();
	void					visibleChanged();
	void					clearTableForResize();
	
private:
	void					updateInternalStructure();
	
	QString					_rawData;
	QChar					_delimiter = ','; // Default comma
	CSVParser*				_parser;  // The parser for CSV parsing
	CSVParser::Grid			_grid; // The parsed data
	bool					_visible = false;
};

#endif // CSVPREVIEWMODEL_H
