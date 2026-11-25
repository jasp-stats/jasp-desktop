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
#include "currencytable.h"

#include <vector>

namespace
{
	struct Entry { const char * code; const char * symbol; };

	///Active ISO 4217 codes followed by the pre-euro ones. The order is the reverse-lookup priority of the symbols:
	///the first entry with a symbol wins, so the most common bearer of a shared symbol comes first.
	const std::vector<Entry> & entries()
	{
		static const std::vector<Entry> entries =
		{
			{ "USD",	"$"		}, { "EUR",	"€"		}, { "GBP",	"£"		}, { "JPY",	"¥"		},
			{ "CNY",	"¥"		}, { "JPY",	"￥"	}, { "CHF",	"CHF"	}, { "CAD",	"C$"	},
			{ "AUD",	"A$"	}, { "NZD",	"NZ$"	}, { "HKD",	"HK$"	}, { "SGD",	"S$"	},
			{ "TWD",	"NT$"	}, { "SEK",	"kr"	}, { "NOK",	"kr"	}, { "DKK",	"kr"	},
			{ "ISK",	"kr"	}, { "PLN",	"zł"	}, { "CZK",	"Kč"	}, { "HUF",	"Ft"	},
			{ "RON",	"lei"	}, { "BGN",	"лв"	}, { "RSD",	"дин."	}, { "MKD",	"ден"	},
			{ "ALL",	"L"		}, { "BAM",	"KM"	}, { "MDL",	"L"		}, { "RUB",	"₽"		},
			{ "UAH",	"₴"		}, { "TRY",	"₺"		}, { "KZT",	"₸"		}, { "AZN",	"₼"		},
			{ "GEL",	"₾"		}, { "AMD",	"֏"		}, { "BYN",	"Br"	}, { "INR",	"₹"		},
			{ "PKR",	"Rs"	}, { "LKR",	"Rs"	}, { "NPR",	"Rs"	}, { "BDT",	"৳"		},
			{ "KRW",	"₩"		}, { "KPW",	"₩"		}, { "BRL",	"R$"	}, { "MXN",	"$"		},
			{ "ARS",	"$"		}, { "CLP",	"$"		}, { "COP",	"$"		}, { "PEN",	"S/"	},
			{ "UYU",	"$U"	}, { "BOB",	"Bs."	}, { "PYG",	"₲"		}, { "VES",	"Bs."	},
			{ "CRC",	"₡"		}, { "SVC",	"₡"		}, { "GTQ",	"Q"		}, { "HNL",	"L"		},
			{ "NIO",	"C$"	}, { "DOP",	"$"		}, { "CUP",	"$"		}, { "ZAR",	"R"		},
			{ "NGN",	"₦"		}, { "KES",	"KSh"	}, { "GHS",	"₵"		}, { "EGP",	"E£"	},
			{ "ETB",	"Br"	}, { "ILS",	"₪"		}, { "SAR",	"﷼"		}, { "QAR",	"ر.ق"	},
			{ "KWD",	"د.ك"	}, { "BHD",	"د.ب"	}, { "OMR",	"ر.ع."	}, { "JOD",	"د.ا"	},
			{ "AED",	"د.إ"	}, { "YER",	"﷼"		}, { "IQD",	"ع.د"	}, { "LBP",	"ل.ل"	},
			{ "LYD",	"ل.د"	}, { "DZD",	"د.ج"	}, { "TND",	"د.ت"	}, { "MAD",	"د.م."	},
			{ "SDG",	"ج.س"	}, { "THB",	"฿"		}, { "VND",	"₫"		}, { "IDR",	"Rp"	},
			{ "MYR",	"RM"	}, { "PHP",	"₱"		}, { "MOP",	"MOP$"	}, { "KHR",	"៛"		},
			{ "LAK",	"₭"		}, { "MMK",	"K"		}, { "MNT",	"₮"		}, { "UZS",	"so'm"	},
			{ "TJS",	"ЅМ"	}, { "TMT",	"m"		}, { "AFN",	"؋"		}, { "BTN",	"Nu."	},
			{ "JMD",	"$"		}, { "TTD",	"$"		}, { "BBD",	"$"		}, { "BSD",	"$"		},
			{ "BZD",	"BZ$"	}, { "GYD",	"$"		}, { "SRD",	"$"		}, { "XCD",	"$"		},
			{ "KYD",	"$"		}, { "FJD",	"FJ$"	}, { "SBD",	"$"		}, { "TOP",	"T$"	},
			{ "VUV",	"VT"	}, { "WST",	"T"		}, { "PGK",	"K"		}, { "MUR",	"₨"		},
			{ "SCR",	"₨"		}, { "MVR",	"Rf"	}, { "MWK",	"MK"	}, { "MZN",	"MT"	},
			{ "ZMW",	"ZK"	}, { "ZWL",	"ZWL$"	}, { "ZWG",	"ZiG"	}, { "BWP",	"P"		},
			{ "LSL",	"L"		}, { "SZL",	"E"		}, { "NAD",	"$"		}, { "ERN",	"Nfk"	},
			{ "SOS",	"Sh"	}, { "SSP",	"£"		}, { "SLE",	"Le"	}, { "GMD",	"D"		},
			{ "CVE",	"$"		}, { "STN",	"Db"	}, { "MRU",	"UM"	}, { "HTG",	"G"		},
			{ "PAB",	"B/."	}, { "FKP",	"£"		}, { "GIP",	"£"		}, { "SHP",	"£"		},
			{ "XOF",	"Fr"	}, { "XAF",	"Fr"	}, { "KMF",	"Fr"	}, { "CDF",	"Fr"	},
			{ "RWF",	"Fr"	}, { "BIF",	"Fr"	}, { "DJF",	"Fr"	}, { "GNF",	"Fr"	},
			{ "NLG",	"ƒ"		}, { "ANG",	"ƒ"		}, { "AWG",	"ƒ"		},
			//The currencies the euro replaced, kept for the auditors among us. The gulden sits with the other
			//"ƒ" bearers above so the one a Dutch user types means NLG, not the Antillean guilder.
			{ "DEM",	"DM"	}, { "FRF",	"₣"		}, { "ITL",	"₤"		},
			{ "ESP",	"₧"		}, { "ATS",	"öS"	}, { "BEF",	"fr"	}, { "LUF",	"fr"	},
			{ "PTE",	"$"		}, { "IEP",	"£"		}, { "FIM",	"mk"	}, { "GRD",	"₯"		},
			{ "CYP",	"£"		}, { "SIT",	"T"		}, { "SKK",	"Sk"	}, { "MTL",	"Lm"	},
			{ "EEK",	"kr"	}, { "LVL",	"Ls"	}, { "LTL",	"Lt"	}, { "HRK",	"kn"	}
		};
		return entries;
	}
}

namespace CurrencyTable
{
	QStringList allCodes()
	{
		QStringList codes;

		for(const Entry & entry : entries())
			if(!codes.contains(entry.code))
				codes.append(entry.code);

		return codes;
	}

	bool isValidCode(const QString & code)
	{
		if(code.size() != 3)
			return false;

		for(const Entry & entry : entries())
			if(code == entry.code)
				return true;

		return false;
	}

	QString symbolForCode(const QString & code)
	{
		for(const Entry & entry : entries())
			if(code == entry.code)
				return entry.symbol;

		return QString();
	}

	QString codeForSymbol(const QString & symbol)
	{
		for(const Entry & entry : entries())
			if(symbol == entry.symbol)
				return entry.code;

		return QString();
	}
}
