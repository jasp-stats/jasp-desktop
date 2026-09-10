Batch
=========

Batch takes the JASP file you have open, swaps its data for another data file, waits for every analysis to finish and writes the results out again. Point it at a single data file or at a whole folder and it does that once per data file, which makes it useful for a monthly report, a set of measurements that arrives per subject, or any other situation where the analyses stay the same and only the data changes.

Everything on this page is also available from the commandline, and the commandline that this page is going to run is shown at the bottom of it. So once a batch does what you want you can copy that line into a terminal or a script and run it there, for instance from a scheduled task. See [the commandline documentation](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/user-guide/command-line-batch-howto.md) for the options that are not on this page, such as the timeout and logging to a file.

## What actually happens

For each data file JASP:

1. loads the JASP file, so every run starts from the same analyses and options;
2. synchronizes the data with the data file, the way the *Synchronisation* button does inside JASP;
3. waits until all analyses have finished refreshing on the new data;
4. exports the results, unless you asked it not to;
5. closes, unless you asked it to stay open.

**Every data file gets its own JASP process.** JASP starts a fresh copy of itself per data file, so one file cannot influence the next: each one starts from the JASP file again.

The JASP file is used *as it is on disk*, so save your workspace first if you made changes you want the batch to use. The JASP you are running the batch from must stay open while the batch runs, it is the one keeping track of it. If you would rather not have that, copy the commandline and run it from a terminal instead.

## JASP file to run

The JASP file you currently have open. Open (or save) the file whose analyses you want to run before starting a batch.

## Data files to run it against

Either one data file, or a folder. A folder is searched recursively, so its subfolders are used as well. Anything JASP can import counts as a data file: `.csv`, `.txt`, `.tsv`, `.sav`, `.zsav`, `.por`, `.xpt`, `.ods`, `.xls`, `.xlsx`, `.dta`, `.sas7bdat`, `.sas7bcat`, `.rdata`, `.rds`, `.mwx` and `.mpx`.

`.jasp`, `.html` and `.pdf` files are not data files, so a folder is allowed to also hold the results of an earlier run.

## Results

The result is named after the data file and written to the output folder, or next to the data file when you leave that empty:

```
measurements/january.csv   ->   reports/january.html
```

One thing to watch: the name is taken up to the **first** dot, not the last. A data file called `january.2026.csv` produces `january.html`, so a folder holding `january.2026.csv` and `january.2027.csv` would have them overwrite each other. Give such files a name without extra dots, or use a separate output folder per run.

With "Do not export" JASP only synchronizes and refreshes. That is a good way to check that a set of data files runs through without errors.

## Options

*Keep JASP open afterwards* leaves the JASP that was started open at the end instead of closing it, so you can look at the result yourself. This works for a folder too, but do keep in mind what it means there: every data file gets its own JASP and they all stay open, so a folder of twenty data files leaves you with twenty JASPs running at the same time. They are also started all at once rather than one after the other, since none of them ever finishes on its own.

Note that this is also the one case where the batch takes the focus: a JASP meant to stay open should be one you can get at. See *While it runs* below.

*Keep columns missing from the data file* changes what happens to a column that the JASP file uses but the new data file does not have. Normally such a column is removed and the analyses using it report that their variable is gone. With this option the column is kept as it is, with the data it already had, so the analyses keep their variables, and the columns of the new data file are added next to the ones already there rather than taking their place.

## While it runs

Batch workers run in hidden windows, so you can keep working in JASP or another application. *Keep JASP open afterwards* opens visible windows for inspection instead.

The output shows progress for each data file. An empty or invalid file, an analysis error, a failed export, or a timeout counts as a failure; processing continues with the next file. JASP warning messages are reported without opening a dialog. The template's original linked data file is not automatically reloaded: the selected batch data is used.

At the end, errors and warnings are listed by data file, followed by the number of successful files, failed files, and distinct JASP warning messages per file. Raw runtime diagnostics (such as Chromium output) are shown separately and are not counted as warnings. An exported report can contain an analysis error and still count as a failed file. With *Keep JASP open afterwards*, the summary reports how many windows were launched; their analyses are not monitored for completion.

*Stop* ends the batch; the data files it already processed keep their results.
