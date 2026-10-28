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

#ifndef BATCHINPUTSMODEL_H
#define BATCHINPUTSMODEL_H

#include <QAbstractListModel>
#include <QSet>
#include <QStringList>

///
/// The data files a batch runs the jasp-file against, as listed on the Batch page.
/// Data files are added by themselves or by adding a folder, in which case all data files in it (and in its subfolders) are listed,
/// so the ones you do not want can be deselected. Every data file is listed once, for whatever added it first.
///
/// On the commandline a folder of which all data files are selected is given as an inputDataDir, which keeps the commandline short
/// and makes it pick up data files that are put in the folder later on. Once one of them is deselected, the selected ones are given one by one.
///
class BatchInputsModel : public QAbstractListModel
{
	Q_OBJECT

public:
	enum Roles
	{
		IsFolderRole = Qt::UserRole + 1,	///< the row of a folder, followed by the rows of its data files
		LabelRole,							///< a data file in a folder relative to that folder, anything else as its full path
		PathRole,
		SelectedRole,						///< for a folder: whether all of its data files are selected
		InFolderRole,						///< a data file that is listed because its folder was added
		RemovableRole,						///< only what was added itself can be removed, a data file in a folder is deselected instead
		SelectedCountRole,					///< for a folder: how many of its data files are selected
		DataFileCountRole					///< for a folder: how many data files are listed for it
	};

	explicit BatchInputsModel(QObject * parent = nullptr);

	int						rowCount(const QModelIndex & parent = QModelIndex())		const override;
	QVariant				data(const QModelIndex & index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()													const override;

	Q_INVOKABLE void		setSelected(int row, bool selected);	///< for the row of a folder this (de)selects all of its data files
	Q_INVOKABLE void		remove(int row);

	QStringList				addDataFiles(const QStringList & paths);	///< returns the names of the files that were not added because they are not data files
	void					addFolder(const QString & path);
	void					refresh();									///< looks in the folders again, data files can have come or gone since they were added

	QStringList				arguments()			const;	///< the data files and inputDataDirs for the commandline
	int						selectedCount()		const;
	bool					isEmpty()			const	{ return _sources.empty(); }
	QString					missingPath()		const;	///< a data file or folder that was added but does not exist anymore, if there is one
	QString					lastAddedPath()		const	{ return _sources.empty() ? QString() : _sources.back().path; }

signals:
	void					selectionChanged();	///< whenever the data files that would be run change

private:
	///A data file or a folder that was added
	struct Source
	{
		QString		path;
		bool		folder;
		QStringList	dataFiles;	///< listed for it: the data file itself, or those in the folder that are not listed for an earlier source already
	};

	///The row of a folder (dataFile is -1) or of one of the data files of a source
	struct Row
	{
		int	source,
			dataFile;
	};

	void					rebuild();										///< looks for the data files of all sources again and lays out the rows
	int						selectedCountOf(const Source & source)	const;
	bool					anyDeselectedIn(const QString & folder)	const;	///< whether a data file somewhere in the folder is deselected, also one listed for another source
	void					selectionOfAllRowsChanged();

	std::vector<Source>		_sources;
	std::vector<Row>		_rows;
	QSet<QString>			_listed,		///< every data file in the list
							_deselected;	///< the data files in the list that should not be run
};

#endif // BATCHINPUTSMODEL_H
