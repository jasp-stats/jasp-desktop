//
// Copyright (C) 2013-2026 University of Amsterdam
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
#ifndef DESKTOPCOMMUNICATOR_H
#define DESKTOPCOMMUNICATOR_H

#include <QObject>
#include <condition_variable>
#include <mutex>
#include <QString>
#include <QLocale>
#include <optional>

///This class only exists to allow signal-slot connections to be made between certain classes in Desktop and in QMLComponents.
/// And to easily split that off for R-only
class DesktopCommunicator : public QObject
{
	Q_OBJECT
public:
	explicit DesktopCommunicator(QObject *parent = nullptr);
	
	static DesktopCommunicator * singleton();

	bool useNativeFileDialog();
	bool engineSandbox();
	char askCsvDelimiter(char autoDelimiter, const QString &data);
	bool queryEncryptionSettings(bool readingMode = false);
	char knownCsvDelimiter() const	{ return _knownCsvDelimiter; }
	void setKnownCsvDelimiter(char d)	{ _knownCsvDelimiter = d; }
	
	///The locale the numbers of the file being imported are written in, as picked in the csv preview (see CsvPreviewModel); nothing when none was picked
	const std::optional<QLocale> &	knownImportLocale() const							{ return _knownImportLocale;	}
	void							setKnownImportLocale(std::optional<QLocale> locale)	{ _knownImportLocale = locale;	}
	
signals:
	void queryEncryptionSettingsSignal(bool readingMode);
	void askCsvDelimiterSignal(const QString &data, char autoDelimiter);
	void currentJaspThemeChanged();
	void uiScaleChanged();
	void interfaceFontChanged();
	bool useNativeFileDialogSignal(); //< For internal use only, `bool useNativeFileDialog();` is what you want
	bool engineSandboxSignal();
	
public slots:
	void encryptionSettingsQueryComplete(bool submit);
	void delimiterChosen(char delimiter);
	
private:
	static DesktopCommunicator * _singleton;

	bool _queryCondition = false;
	bool _querySubmitted = false;
	bool _csvCondition = false;
	char _csvSubmitted = '\0';
	char _knownCsvDelimiter = '\0';
	std::optional<QLocale> _knownImportLocale;

	std::mutex _queryLock;
	std::mutex _csvLock;

	std::condition_variable _query_cv;
	std::condition_variable _csv_cv;
};

#endif // DESKTOPCOMMUNICATOR_H
