#ifndef UTILENUMS_H
#define UTILENUMS_H
#include "enumutilities.h"

DECLARE_ENUM(FileTypeBase,	jasp = 0, html, csv, txt, tsv, sav, ods, pdf, sas7bdat, sas7bcat, por, xpt, dta, database, empty, unknown );

//const QStringList Database::dbTypes() const should be updated if DbType is changed.
DECLARE_ENUM(DbType,		NOTCHOSEN, QDB2, /*QIBASE,*/ QMYSQL, QOCI, QODBC, QPSQL, QSQLITE /*, QSQLITE2, QTDS*/ );

#endif // UTILENUMS_H
