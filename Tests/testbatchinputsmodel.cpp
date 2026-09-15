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

#include <QtTest>
#include <QTemporaryDir>

#include "widgets/filemenu/batchinputsmodel.h"
#include "parsedarguments.h"
#include "utilities/qutils.h"

namespace
{
	QString createFile(const QTemporaryDir & dir, const QString & name)
	{
		QString path = dir.path() + "/" + name;
		QDir().mkpath(QFileInfo(path).absolutePath());

		QFile file(path);
		if(file.open(QIODevice::WriteOnly))
			file.write("a,b\n1,2\n");

		return QFileInfo(path).absoluteFilePath();
	}

	QString folderIn(const QTemporaryDir & dir, const QString & name)
	{
		return QFileInfo(dir.path() + "/" + name).absoluteFilePath();
	}

	QString inputDataDir()
	{
		return tq(ParsedArguments::inputDataDirArg);
	}

	QStringList labels(const BatchInputsModel & model)
	{
		QStringList labels;

		for(int row = 0; row < model.rowCount(); row++)
			labels << model.data(model.index(row), BatchInputsModel::LabelRole).toString();

		return labels;
	}
}

class TestBatchInputsModel : public QObject
{
	Q_OBJECT

private slots:
	void folderListsItsDataFiles()
	{
		QTemporaryDir dir;
		createFile(dir, "data/a.csv");
		createFile(dir, "data/sub/b.sav");
		createFile(dir, "data/results.html");
		const QString data = folderIn(dir, "data");

		BatchInputsModel model;
		model.addFolder(data);

		// The folder first, then its data files relative to it, without what is not a data file
		QCOMPARE(labels(model), QStringList({ data, "a.csv", "sub/b.sav" }));
		QVERIFY( model.data(model.index(0), BatchInputsModel::IsFolderRole).toBool());
		QVERIFY( model.data(model.index(0), BatchInputsModel::RemovableRole).toBool());
		QVERIFY( model.data(model.index(1), BatchInputsModel::InFolderRole).toBool());
		QVERIFY(!model.data(model.index(1), BatchInputsModel::RemovableRole).toBool());
		QCOMPARE(model.selectedCount(), 2);
	}

	void wholeFolderIsGivenAsInputDataDir()
	{
		QTemporaryDir dir;
		const QString	a		= createFile(dir, "data/a.csv"),
						b		= createFile(dir, "data/b.csv"),
						c		= createFile(dir, "data/c.csv"),
						data	= folderIn(dir, "data");

		BatchInputsModel model;
		model.addFolder(data);
		QCOMPARE(model.arguments(), QStringList({ inputDataDir(), data }));

		// Deselecting one of its data files gives the others one by one
		model.setSelected(2, false); // b.csv
		QCOMPARE(model.arguments(), QStringList({ a, c }));
		QCOMPARE(model.data(model.index(0), BatchInputsModel::SelectedCountRole).toInt(), 2);
		QVERIFY(!model.data(model.index(0), BatchInputsModel::SelectedRole).toBool());

		model.setSelected(2, true);
		QCOMPARE(model.arguments(), QStringList({ inputDataDir(), data }));

		// The row of the folder (de)selects all of them
		model.setSelected(0, false);
		QCOMPARE(model.selectedCount(), 0);
		QVERIFY(model.arguments().isEmpty());

		model.setSelected(0, true);
		QCOMPARE(model.arguments(), QStringList({ inputDataDir(), data }));
	}

	void dataFileIsListedOnce()
	{
		QTemporaryDir dir;
		const QString	a		= createFile(dir, "data/a.csv"),
						b		= createFile(dir, "data/sub/b.csv"),
						data	= folderIn(dir, "data"),
						sub		= folderIn(dir, "data/sub");

		BatchInputsModel model;
		QVERIFY(model.addDataFiles({ a }).isEmpty());
		model.addFolder(data);
		model.addFolder(sub);

		// a.csv stays where it was added first, and data/sub has nothing of its own left
		QCOMPARE(labels(model), QStringList({ a, data, "sub/b.csv", sub }));
		QCOMPARE(model.selectedCount(), 2);
		QCOMPARE(model.arguments(), QStringList({ a, inputDataDir(), data }));

		// a.csv is in data, so once it is deselected data cannot be given as a whole anymore
		model.setSelected(0, false);
		QCOMPARE(model.arguments(), QStringList({ b }));

		// Adding a listed data file again selects it again, without listing it twice
		QVERIFY(model.addDataFiles({ a }).isEmpty());
		QCOMPARE(model.rowCount(), 4);
		QCOMPARE(model.arguments(), QStringList({ a, inputDataDir(), data }));
	}

	void removingMovesDataFilesToWhatElseHasThem()
	{
		QTemporaryDir dir;
		createFile(dir, "data/sub/b.csv");
		const QString	data	= folderIn(dir, "data"),
						sub		= folderIn(dir, "data/sub");

		BatchInputsModel model;
		model.addFolder(data);
		model.addFolder(sub);
		QCOMPARE(labels(model), QStringList({ data, "sub/b.csv", sub }));

		// A data file in a folder is deselected, not removed
		model.remove(1);
		QCOMPARE(model.rowCount(), 3);

		model.remove(0);
		QCOMPARE(labels(model), QStringList({ sub, "b.csv" }));
		QCOMPARE(model.arguments(), QStringList({ inputDataDir(), sub }));
	}

	void onlyDataFilesAreAdded()
	{
		QTemporaryDir dir;
		const QString	a		= createFile(dir, "a.csv"),
						report	= createFile(dir, "report.pdf");

		BatchInputsModel model;
		QCOMPARE(model.addDataFiles({ a, report }), QStringList({ "report.pdf" }));
		QCOMPARE(labels(model), QStringList({ a }));
		QCOMPARE(model.arguments(), QStringList({ a }));
	}

	void refreshSeesWhatChangedInTheFolder()
	{
		QTemporaryDir dir;
		const QString	a		= createFile(dir, "data/a.csv"),
						b		= createFile(dir, "data/b.csv"),
						data	= folderIn(dir, "data");

		BatchInputsModel model;
		model.addFolder(data);
		model.setSelected(1, false); // a.csv

		const QString c = createFile(dir, "data/c.csv");
		QVERIFY(QFile::remove(b));
		model.refresh();

		// b.csv is gone, c.csv came in selected, and a.csv is still deselected
		QCOMPARE(labels(model), QStringList({ data, "a.csv", "c.csv" }));
		QCOMPARE(model.arguments(), QStringList({ c }));
	}

	void missingPath()
	{
		QTemporaryDir dir;
		const QString a = createFile(dir, "a.csv");

		BatchInputsModel model;
		model.addDataFiles({ a });
		QVERIFY(model.missingPath().isEmpty());

		QVERIFY(QFile::remove(a));
		QCOMPARE(model.missingPath(), a);
	}
};

QTEST_GUILESS_MAIN(TestBatchInputsModel)
#include "testbatchinputsmodel.moc"
