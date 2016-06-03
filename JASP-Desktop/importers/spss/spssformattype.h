#ifndef SPSSFORMATTYPE_H
#define SPSSFORMATTYPE_H

/*
 * Takem from PSPP Developer's Guide release 0.10.2
 */
typedef enum e_formats
{
	format_reservered_1 = 0,
	format_A,
	format_AHEX,
	format_COMMA,
	format_DOLLAR,
	format_F,
	format_IB,
	format_PIBHEX,
	format_P,
	format_PIB,
	format_PK,
	format_RB,
	format_RBHEX,
	format_reservered_2,
	format_reservered_3,
	format_Z,
	format_N,
	format_E,
	format_reservered_4,
	format_reservered_5,
	format_DATE,
	format_TIME,
	format_DATETIME,
	format_ADATE,
	format_JDATE,
	format_DTIME,
	format_WKDAY,
	format_MONTH,
	format_MOYR,
	format_QYR,
	format_WKYR,
	format_PCT,
	format_DOT,
	format_CCA,
	format_CCB,
	format_CCC,
	format_CCD,
	format_CCE,
	format_EDATE,
	format_SDATE,
	_num_formatTypes
} FormatTypes;

#endif // SPSSFORMATTYPE_H

