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
#ifndef NUMBERSINLOCALES_H
#define NUMBERSINLOCALES_H

#include <QLocale>
#include <QString>
#include <cmath>
#include <limits>
#include <vector>

///Numbers written with dots, commas and spaces in every role they can play, and what each of them is in a file written in English, German or French.
///Shared by the tests of the csv preview and of the csv import, because the preview has to show exactly what the import will make of a file.
namespace NumbersInLocales
{
	///What a text that is no number at all stays: text
	const double text = std::numeric_limits<double>::quiet_NaN();

	///The three languages the tests read and show numbers in:
	///English writes 1,234.5, German 1.234,5 and French 1 234,5 (with a narrow no-break space).
	inline const std::vector<QLocale> & locales()
	{
		static const std::vector<QLocale> locales = { QLocale(QLocale::English, QLocale::UnitedStates), QLocale(QLocale::German, QLocale::Germany), QLocale(QLocale::French, QLocale::France) };
		return locales;
	}

	struct Sample
	{
		const char *	written;	///< As it stands in the file, in UTF-8
		double			english,	///< What it is in a file written in English
						german,		///< ... in German
						french;		///< ... in French

		double readIn(const QLocale & locale) const
		{
			switch(locale.language())
			{
			case QLocale::English:	return english;
			case QLocale::German:	return german;
			case QLocale::French:	return french;
			default:				return text;
			}
		}
	};

	///A text that is no number in the language of the file is read the way C writes numbers (see QColumnUtils::readNumber),
	///which is why "1234.5" is a number in all three languages. A text that is no number in C either stays text.
	inline const std::vector<Sample> & samples()
	{
		static const std::vector<Sample> samples =
		{
			//	written						english			german			french
			{	"1234",						1234,			1234,			1234		},
			{	"-1234",					-1234,			-1234,			-1234		},
			{	"1234.5",					1234.5,			1234.5,			1234.5		},	//A German or French dot that groups nothing is no number there, but C reads it
			{	"1234,5",					text,			1234.5,			1234.5		},
			{	"0.5",						0.5,			0.5,			0.5			},
			{	"0,5",						text,			0.5,			0.5			},
			{	".5",						0.5,			0.5,			0.5			},
			{	",5",						text,			0.5,			0.5			},
			{	"1.234",					1.234,			1234,			1.234		},	//Groups thousands in German only
			{	"1,234",					1234,			1.234,			1.234		},	//Groups thousands in English only
			{	"1,23",						text,			1.23,			1.23		},	//Three digits follow a thousands separator, so in English this is none
			{	"1,234.56",					1234.56,		text,			text		},
			{	"1.234,56",					text,			1234.56,		text		},	//The French group with a space, not a dot
			{	"-1,234.56",				-1234.56,		text,			text		},
			{	"-1.234,56",				text,			-1234.56,		text		},
			{	"1,234,567.89",				1234567.89,		text,			text		},
			{	"1.234.567,89",				text,			1234567.89,		text		},
			{	"1 234,56",					text,			text,			1234.56		},	//French with the space a keyboard types,
			{	"1\xE2\x80\xAF" "234,56",	text,			text,			1234.56		},	//the narrow no-break space (U+202F) French is written with,
			{	"1\xC2\xA0" "234,56",		text,			text,			1234.56		},	//and the no-break space (U+00A0) it was written with before
			{	"1 234.56",					text,			text,			text		},
			{	"1.5e3",					1500,			1500,			1500		},
			{	"1,5e3",					text,			1500,			1500		},
			{	"1.2.3",					text,			text,			text		},
			{	"1,2,3",					text,			text,			text		},
		};

		return samples;
	}

	///How a number is shown in an interface set to locale, the way the data and the csv preview show it (see QColumnUtils::setCallbacksAndDefaultLocale)
	inline QString shownIn(const QLocale & locale, double number, bool thousandSeparators)
	{
		QLocale showWith(locale);

		if(!thousandSeparators)
			showWith.setNumberOptions(showWith.numberOptions() | QLocale::OmitGroupSeparator);

		return showWith.toString(number, 'g', 10);
	}
}

#endif // NUMBERSINLOCALES_H
