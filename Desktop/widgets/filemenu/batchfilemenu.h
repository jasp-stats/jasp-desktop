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

#ifndef BATCHFILEMENU_H
#define BATCHFILEMENU_H

#include <QProcess>
#include <QStringList>

#include "filemenuobject.h"
#include "exporttype.h"
#include "batchinputsmodel.h"

///
/// The "Batch" page of File/Sync Data, a frontend for the batch commandline described in
/// Docs/user-guide/command-line-batch-howto.md: it collects the arguments, shows the exact
/// commandline it is going to run and then runs JASP with it, showing the output while it goes.
///
class BatchFileMenu : public FileMenuObject
{
	Q_OBJECT

	Q_PROPERTY(QString	jaspFile					READ jaspFile													NOTIFY jaspFileChanged					)
	Q_PROPERTY(bool		jaspFileModified			READ jaspFileModified											NOTIFY jaspFileChanged					)
	Q_PROPERTY(BatchInputsModel *	inputs			READ inputs														CONSTANT								)
	Q_PROPERTY(QString	outputFolder				READ outputFolder					WRITE setOutputFolder		NOTIFY outputFolderChanged				)
	Q_PROPERTY(int		exportTypeIndex				READ exportTypeIndex				WRITE setExportTypeIndex	NOTIFY exportTypeIndexChanged			)
	Q_PROPERTY(bool		keepJASPOpen				READ keepJASPOpen					WRITE setKeepJASPOpen		NOTIFY keepJASPOpenChanged				)
	Q_PROPERTY(bool		keepMissingColsWhenSyncing	READ keepMissingColsWhenSyncing		WRITE setKeepMissingColsWhenSyncing	NOTIFY keepMissingColsWhenSyncingChanged)
	Q_PROPERTY(QString	commandLine					READ commandLine												NOTIFY commandLineChanged				)
	Q_PROPERTY(QString	problem						READ problem													NOTIFY commandLineChanged				)
	Q_PROPERTY(bool		readyToRun					READ readyToRun													NOTIFY commandLineChanged				)
	Q_PROPERTY(bool		running						READ running													NOTIFY runningChanged					)
	Q_PROPERTY(QString	output						READ output														NOTIFY outputChanged					)

public:
	explicit				BatchFileMenu(FileMenu * parent);

	Q_INVOKABLE void		browseDataFiles();
	Q_INVOKABLE void		browseDataFolder();
	Q_INVOKABLE void		browseOutputFolder();
	Q_INVOKABLE void		runBatch();
	Q_INVOKABLE void		stopBatch();
	Q_INVOKABLE void		clearOutput();
	Q_INVOKABLE void		copyCommandLine()			const;

	QString					jaspFile()					const;
	bool					jaspFileModified()			const;
	BatchInputsModel	*	inputs()					const	{ return _inputs;						}
	const QString		&	outputFolder()				const	{ return _outputFolder;					}
	int						exportTypeIndex()			const;
	bool					keepJASPOpen()				const	{ return _keepJASPOpen;					}
	bool					keepMissingColsWhenSyncing()const	{ return _keepMissingColsWhenSyncing;	}
	QString					commandLine()				const;
	QString					problem()					const;
	bool					readyToRun()				const	{ return problem().isEmpty() && !running();	}
	bool					running()					const	{ return _process;						}
	const QString		&	output()					const	{ return _output;						}

	void					setOutputFolder(				const QString &	outputFolder				);
	void					setExportTypeIndex(				int				exportTypeIndex				);
	void					setKeepJASPOpen(				bool			keepJASPOpen				);
	void					setKeepMissingColsWhenSyncing(	bool			keepMissingColsWhenSyncing	);

	void					refresh();	///< makes sure the info taken from the loaded workspace is up to date, called whenever the Batch button is selected

signals:
	void					jaspFileChanged();
	void					outputFolderChanged();
	void					exportTypeIndexChanged();
	void					keepJASPOpenChanged();
	void					keepMissingColsWhenSyncingChanged();
	void					commandLineChanged();
	void					runningChanged();
	void					outputChanged();

private slots:
	void					readProcessOutput();
	void					processFinished(int exitCode, QProcess::ExitStatus exitStatus);
	void					processErrorOccurred(QProcess::ProcessError error);

private:
	QStringList				arguments()										const;	///< the arguments JASP is started with, in the same order as the documentation lists them
	QString					browseStartFolder()								const;	///< the folder the browse-dialogs start in: where the data file or folder added last is, or else the folder of the jasp-file
	void					appendOutput(const QString & text);

	static QString			quoteIfNeeded(const QString & argument);
	static QStringList		dataFileExtensions();									///< the extensions of what JASP counts as a data file, the same it looks for in a folder

	///< The exporttypes offered on the page, in the order the dropdown in Batch.qml shows them
	static const std::vector<ExportType>	exportTypes;

#ifdef __APPLE__
	///< Keeps a starting JASP from making itself the foreground application, see runBatch()
	static const QString					noFocusStealingEnvVar;
#endif

	QProcess			*	_process					= nullptr;
	BatchInputsModel	*	_inputs						= nullptr;	///< the data files to run the jasp-file against
	QString					_outputFolder,
							_output;
	ExportType				_exportType					= ExportType::Html;
	bool					_keepJASPOpen				= false,
							_keepMissingColsWhenSyncing	= false;
};

#endif // BATCHFILEMENU_H
