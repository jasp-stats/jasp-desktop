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
#include <QLocale>
#include "columnutils.h"

///Backs the Data Preview window: it shows how a csv file will be split into columns and how its numbers will be read.
///The locale used for reading is owned here, it starts out as the locale of the interface but the user can pick another one
///for this import only, without touching their preferences. See setLanguage/setTerritory and importLocale.
class CsvPreviewModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(QString		rawData			READ rawData		WRITE setRawData	NOTIFY rawDataChanged		)
	Q_PROPERTY(QChar		delimiter		READ delimiter		WRITE setDelimiter	NOTIFY delimiterChanged		)
	Q_PROPERTY(bool			visible			READ visible		WRITE setVisible	NOTIFY visibleChanged		)
	Q_PROPERTY(QStringList	languages		READ languages							NOTIFY languagesChanged		)
	Q_PROPERTY(bool			moreLanguages	READ moreLanguages	WRITE setMoreLanguages	NOTIFY moreLanguagesChanged		)
	Q_PROPERTY(QStringList	territories		READ territories						NOTIFY territoriesChanged	)
	Q_PROPERTY(QString		language		READ language		WRITE setLanguage	NOTIFY languageChanged		)
	Q_PROPERTY(QString		territory		READ territory		WRITE setTerritory	NOTIFY territoryChanged		)
	Q_PROPERTY(QString		parseExample	READ parseExample						NOTIFY parseExampleChanged	)

public:
	explicit CsvPreviewModel(QObject *parent = nullptr);

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

	QStringList				languages()		const;
	QStringList				territories()	const { return _territories;	}
	bool					moreLanguages()	const { return _moreLanguages;		}
	void					setMoreLanguages(bool moreLanguages);
	QString					language()		const { return _language;		}
	QString					territory()		const { return _territory;		}
	QString					parseExample()	const;

	void					setLanguage(	const QString & language);
	void					setTerritory(	const QString & territory);

	const QLocale &			importLocale()	const { return _importLocale;	}

public slots:
	void					updateLocale();
	void					resetLocaleToInterface();	///< Back to reading numbers the way the rest of JASP does, which is where every import starts

signals:
	void					rawDataChanged();
	void					delimiterChanged();
	void					visibleChanged();
	void					clearTableForResize();
	void					territoriesChanged();
	void					languagesChanged();
	void					moreLanguagesChanged();
	void					languageChanged();
	void					territoryChanged();
	void					parseExampleChanged();
	
private:
	void					updateInternalStructure();
	void					_setLocale(const QLocale & locale);			///< The one place the chosen locale changes, everything shown follows from it
	QLocale					_localeForLanguage()				const;	///< Resolves whatever the language dropdown currently shows
	QString					_languageNameFor(const QLocale & l)	const;	///< How that locale is named in the list the dropdown currently offers
	void					_applyImportLocale();			///< Makes ColumnUtils read numbers with _importLocale, for the preview here and for the import that follows

	QString					_rawData;
	QChar					_delimiter = ','; // Default comma
	QList<QList<QString>>	_grid; // The parsed data
	bool					_visible	= false,
							_moreLanguages	= false;	///< Off means only the languages JASP itself speaks are offered, on means every language Qt knows plus a territory

	QLocale					_importLocale;
	QString					_language,
							_territory;
	QStringList				_territories;	///< Depends on _language, the languages themselves come straight from LanguageModel
};

#endif // CSVPREVIEWMODEL_H
