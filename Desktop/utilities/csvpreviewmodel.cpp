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
#include "utilities/languagemodel.h"
#include <QScopedValueRollback>

CsvPreviewModel::CsvPreviewModel(QObject *parent) : QAbstractTableModel(parent)
{
	resetLocaleToInterface();
}

QStringList CsvPreviewModel::languages() const
{
	//LanguageModel already has both of these, no reason to enumerate anything here.
	//Hundreds of languages are hard to pick from, so this offers the same short list as Preferences/Interface until More languages is ticked.
	if(!LanguageModel::lang())
		return QStringList();

	return _moreLanguages ? LanguageModel::lang()->altLanguages() : LanguageModel::lang()->languageEntryNames();
}

QLocale CsvPreviewModel::_localeForLanguage() const
{
	if(!LanguageModel::lang())
		return QLocale::c();

	//The two lists name their languages differently: Preferences writes "en - American English", the complete range uses native names only
	return _moreLanguages	? LanguageModel::lang()->localeForNames(_language, _territory)
						: LanguageModel::lang()->localeForEntryName(_language);
}

QString CsvPreviewModel::_languageNameFor(const QLocale & locale) const
{
	if(!LanguageModel::lang())
		return "";

	if(_moreLanguages)
		return locale.nativeLanguageName();

	const QString entryName = LanguageModel::lang()->entryNameForLocale(locale);

	//A language JASP is not translated into has no entry in the short list, so fall back on the language of the interface
	return entryName.isEmpty() ? LanguageModel::lang()->currentLanguage() : entryName;
}

void CsvPreviewModel::setMoreLanguages(bool moreLanguages)
{
	if(_moreLanguages == moreLanguages)
		return;

	QScopedValueRollback<bool> settingLocale(_settingLocale, true); //See _setLocale

	const QLocale wasUsing = _importLocale;

	_moreLanguages = moreLanguages;

	emit moreLanguagesChanged();
	emit languagesChanged();	//The dropdown switches between the short and the complete list

	//Both lists name the same locale differently, so the names have to be derived again either way
	QLocale keepUsing = wasUsing;

	//And unticking More languages can leave behind a language that the short list does not offer at all
	if(!_moreLanguages && LanguageModel::lang() && LanguageModel::lang()->entryNameForLocale(wasUsing).isEmpty())
		keepUsing = _interfaceLocale();

	_setLocale(keepUsing);
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
	resetLocaleToInterface(); //Every import starts from the locale the user works in, whatever they chose for the previous file
}

void CsvPreviewModel::resetLocaleToInterface()
{
	QScopedValueRollback<bool> settingLocale(_settingLocale, true); //See _setLocale

	emit languagesChanged(); //Preferences may have been given another language since the previous import

	_setLocale(_interfaceLocale());
}

QLocale CsvPreviewModel::_interfaceLocale() const
{
	//The locale the rest of JASP reads numbers with, which is the alternative locale of the preferences when one is set.
	//Without a LanguageModel there is no interface to take it from (unit tests), and then plain C is the honest default
	return LanguageModel::lang() ? LanguageModel::lang()->currentLocale() : QLocale::c();
}

void CsvPreviewModel::_setLocale(const QLocale & locale)
{
	//The dropdowns write whatever they show back through setLanguage and setTerritory, also while they are still catching up
	//with a list that changed underneath them and briefly show its first entry. That must not overrule the locale set here.
	QScopedValueRollback<bool> settingLocale(_settingLocale, true);

	//Single point where the locale changes: the two names shown in the dropdowns are derived from it, never the other way around
	_importLocale	= locale;
	_language		= _languageNameFor(locale);
	_territories	= LanguageModel::lang() ? LanguageModel::lang()->territoriesForLanguage(locale.nativeLanguageName()) : QStringList();
	_territory		= locale.nativeTerritoryName();

	emit languageChanged();
	emit territoriesChanged();
	emit territoryChanged();

	_applyImportLocale();
}

void CsvPreviewModel::_applyImportLocale()
{
	updateInternalStructure();

	emit parseExampleChanged();
}

void CsvPreviewModel::setLanguage(const QString & language)
{
	if(_settingLocale || _language == language || language == "")
		return;

	_language = language;

	_setLocale(_localeForLanguage());	//Which also picks the default territory of that language, a territory belongs to a language
}

void CsvPreviewModel::setTerritory(const QString & territory)
{
	if(_settingLocale || _territory == territory || territory == "")
		return;

	_territory		= territory;
	_importLocale	= _localeForLanguage();

	emit territoryChanged();

	_applyImportLocale();
}

bool CsvPreviewModel::_readNumber(const QString & text, double & number) const
{
	//Exactly what CSVImportColumn::valueLookup does with it during the import
	return QColumnUtils::stringToDoubleFor(_importLocale)(fq(text), number) || QColumnUtils::getDoubleValue(text, number, true);
}

QString CsvPreviewModel::parseExample() const
{
	//Values picked to show what the decimal- and group-separators of the chosen locale do to a number
	static const QStringList samples = { "86.298", "86,298", "1.234,56", "1,234.56" };

	QStringList lines;

	for(const QString & sample : samples)
	{
		double	value		= 0;
		bool	isNumber	= _readNumber(sample, value);

		lines.push_back(sample + "  \u2192  " + (isNumber ? QLocale::c().toString(value, 'g', 12) : tr("text")));
	}

	return lines.join("\n");
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
		if (_readNumber(val, dblVal))
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
	{
		//Hand the locale to the importer, which is waiting on another thread for delimiterChosen
		DesktopCommunicator::singleton()->setKnownImportLocale(_importLocale);
		DesktopCommunicator::singleton()->delimiterChosen(_delimiter.toLatin1());
	}
}
