# Running a JASP file against several data files from the command line

JASP can take an existing `.jasp` file, swap its data for another data file, wait for every
analysis to finish, and write the results out again. Point it at a list of data files (or a whole
folder) and it does that once per file, which makes it useful for a monthly report, a set of
measurements that arrives per subject, or any other situation where the analyses stay the same and
only the data changes.

These options are part of the JASP Pro feature set and are ignored by a build that does not run in
pro mode.

You can also put such a run together inside JASP itself, under *File* > *Sync Data* > *Batch*. That
page has the options below that are about which files to use, and shows the commandline it is going
to run, so it doubles as a way to write the line you want to use here.

## The short version

```
JASP <jasp file> [<data file> ...] [options]
```

Take `weights.jasp`, run it against three data files and drop an HTML report next to each of them:

```
JASP weights.jasp january.csv february.csv march.csv
```

Same thing for every data file in a folder (and its subfolders), as PDF, collected in one place:

```
JASP weights.jasp --inputDataDir ./measurements --outputDir ./reports --exportType=Pdf
```

## What actually happens

For each data file JASP:

1. loads the `.jasp` file, so every run starts from the same analyses and options;
2. synchronizes the data with the data file, the way the *Synchronisation* button does inside JASP;
3. waits until all analyses have finished refreshing on the new data;
4. exports the results, unless you asked it not to;
5. closes, unless you asked it to stay open.

**Every data file gets its own JASP process.** JASP starts a fresh copy of itself per data file, so
one file cannot influence the next: each one starts from the `.jasp` file again. The only exception
is a single data file combined with `--keepJASPOpen`, which is handled in the JASP you started.

## Options

| Option | What it does |
| --- | --- |
| `--inputDataDir <folder>` | Use every data file in this folder *and its subfolders* instead of naming them one by one. |
| `--outputDir <folder>` | Write all results here. Without it, each result lands next to its own data file. The folder is created if it does not exist. |
| `--exportType=Html/Pdf/Jasp/No` | Default is Html. If `No` is used, only synchronize and refresh is done: can be useful to check that a set of data files runs through without errors. |
| `--keepMissingColsWhenSyncing` | Keep columns that the new data file does not have, instead of removing them. See below. |
| `--keepJASPOpen` | Leave JASP open at the end instead of closing it. Only meaningful with a single data file. |
| `--save` | Save the `.jasp` file after refreshing. Same as `--exportType=Jasp` |
| `--timeOut=<minutes>` | How long to wait for the analyses of one data file. Default is 10. |
| `--logToFile` | Write logging to a file, for when a run does something you did not expect. |

## Which files count as data files

Anything JASP can import: `.csv`, `.txt`, `.tsv`, `.sav`, `.zsav`, `.por`, `.xpt`, `.ods`, `.xls`,
`.xlsx`, `.dta`, `.sas7bdat`, `.sas7bcat`, `.rdata`, `.rds`, `.mwx` and `.mpx`.

`.jasp`, `.html` and `.pdf` files are not data files, so `--inputDataDir` skips them and you can
safely point it at a folder that also holds the results of an earlier run.

## Where the results end up

The result is named after the data file, in the folder given by `--outputDir`, or next to the data
file when that option is absent:

```
measurements/january.csv   ->   reports/january.html
```

One thing to watch: the name is taken up to the **first** dot, not the last. A data file called
`january.2026.csv` produces `january.html`, so a folder holding `january.2026.csv` and
`january.2027.csv` would have them overwrite each other. Give such files a name without extra dots,
or use `--outputDir` per run.

## Keeping columns that disappeared

By default, a column that the `.jasp` file uses but the new data file does not have is removed, and
the analyses using it will report that their variable is gone.

With `--keepMissingColsWhenSyncing` those columns are kept instead, so the
analyses keep their variables. The columns of the new data file
are added next to the ones already there rather than taking their place.

Because each data file gets its own JASP process, this starts fresh every time.

## Exit codes

Meant for scripting; `0` always means everything went through.

| Code | Meaning |
| --- | --- |
| `0` | All data files were processed and exported. |
| `1` | At least one data file failed, or there was nothing to synchronize with. |
| `3` | The `.jasp` file itself could not be opened. |

A data file counts as failed when its JASP could not be started, exited with an error, or did not
finish within the timeout (it is stopped after `--timeOut` minutes plus ten seconds of grace).
The names of the failing files are written to standard error, followed by a count.

## Examples

Check that a set of data files still runs, without producing reports:

```
JASP weights.jasp --inputDataDir ./measurements --exportType=No
```

Give slow analyses more room and keep the refreshed data:

```
JASP weights.jasp big-january.csv big-february.csv --timeOut=45 --exportType=Jasp
```

Look at the result of a single data file in JASP itself instead of exporting it:

```
JASP weights.jasp january.csv --exportType=No --keepJASPOpen
```

From a shell script, acting on the exit code:

```bash
if JASP weights.jasp --inputDataDir ./measurements --outputDir ./reports; then
    echo "all reports written"
else
    echo "at least one data file failed, see the output above"
fi
```

## When something goes wrong

* Add `--logToFile` and look at the log if a run fails without a clear message.
* Use `--exportType=No` together with `--keepJASPOpen` and one data file to inspect the
  synchronized data by hand.
