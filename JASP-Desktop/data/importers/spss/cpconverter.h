#ifndef CPCONVERTER_H
#define CPCONVERTER_H


class CPConverter
{
public:

	/*
	 * Known code pages, as understood by IBM/Windows.
	 */
	typedef enum e_codepages
	{
		us_ascii	= 20127,	// 7 bit ASCII
		utf_7		= 650000,	// Unicode 7 -bit coded.
		utf_8		= 650001,	// Unicode 8 -bit coded.
		// No 16 bit!
//		utf_16le	= 1200,		// Unicode 16 Little Endain
//		utf_16be	= 1201,		// Unicode 16 Big Endain
		win1250		= 1250,		// Windows Latin 2 Central Europe.
		win1251 	= 1251,		// Windows Cyrillic
		win1252 	= 1252,		// Windows Latin 1 / Western European
		win1253		= 1253,		// Windows Greek
		win1254		= 1254,		// Windows Turkish
		win1255		= 1255,		// Windows-Hebrew
		win1256		= 1256,		// Windows-Arabic
		win1257 	= 1257,		// Windows Baltic
		win1258 	= 1258,		// Windows-Vietnamese
		dos437		= 437,		// IBM/DOS 437 (US)
		dos720		= 720,		// IBM/DOS 720 (Arabic)
		dos737		= 737,		// IBM/DOS 737 (Greek)
		dos755		= 755,		// IBM/DOS 755 (Baltic rim)
		dos850		= 850,		// IBM/DOS 850 (western Europe aka Multilingual Latin I)
		dos852		= 852,		// IBM/DOS 852 (central Europe aka Multilingual Latin II)
		dos855		= 855,		// IBM/DOS 855 (Cyrillic)
		dos857		= 857,		// IBM/DOS 857 (Turkish)
		dos858		= 858,		// IBM/DOS 858 (Central Europe/Multilingual Latin I + With €)
		dos860		= 860,		// IBM/DOS 860 (Portuguese)
		dos861		= 861,		// IBM/DOS 861 (Icelandic)
		dos862		= 862,		// IBM/DOS 862 (Hebrew)
		dos863		= 863,		// IBM/DOS 863 (French Canadian)
		dos865		= 865,		// IBM/DOS 865 (Nordic)
		dos866		= 866,		// IBM/DOS 866 (Cyrillic)
		dos869		= 869,		// IBM 869 (Greek)
		IS08859_1	= 28591,	// ISO-8559-1 aka Windows 28591
		ISO8859_2	= 28592,	// ISO-8559-2 aka Windows 28592
		ISO8859_3	= 28593,	// ISO-8559-3 aka Windows 28593
		ISO8859_4	= 28594,	// ISO-8559-4 aka Windows 28594
		ISO8859_5	= 28595,	// ISO-8559-5 aka Windows 28595
		ISO8859_6	= 28596,	// ISO-8559-6 aka Windows 28596
		ISO8859_7	= 28597,	// ISO-8559-7 aka Windows 28597
		ISO8859_8	= 28598,	// ISO-8559-8 aka Windows 28598
		ISO8859_9	= 28599,	// ISO-8559-9 aka Windows 28599
		ISO8859_13	= 28603,	// ISO-8559-13 aka Windows 28603
		ISO8859_15	= 28605,	// ISO-8559-15 aka Windows 28605
		macintosh	= 10000,	// Mac (US) macos-0_2-10.2
		mac_greek	= 10006,	// Mac (Greek) macos-6_2-10.2
		mac_cyrillic= 10007,	// Mac (Cyrillic/Ukrian) macos-7_3-10.2
		mac_c_europe= 10029,	// Mac (Central Europe) macos-29-10.2
		mac_turkish = 10081,	// Mac (Turkish) macos-35-10.2
		EBCDIC_US	= 37,		// EBCDIC (US-CANADA)
		_unknown_codePage = 0
	} CodePage;

	/*
	 * MiB's as understood by QTextCodec
	 * (from http://www.iana.org/assignments/character-sets/character-sets.xhtml)
	 */
	typedef enum e_mibs
	{
		US_ASCII = 3,		// US ASCII (7bit)
		ISO_8859_1 = 4,
		ISO_8859_2 = 5,
		ISO_8859_3 = 6,
		ISO_8859_4 = 7,
		ISO_8859_5 = 8,
		ISO_8859_6 = 9,
		ISO_8859_7 = 10,
		ISO_8859_8 = 11,
		ISO_8859_9 = 12,
		ISO_8859_10 = 13,
		ISO_6937_2_add = 14,
		JIS_X0201 = 15,
		JIS_Encoding = 16,
		Shift_JIS = 17,
		EUC_JP = 18,
		Fixed_Width_for_Japanese = 19, 	// Extended_UNIX_Code_Fixed_Width_for_Japanese
		BS_4730 = 20,
		SEN_850200_C = 21,
		IT = 22,
		ES = 23,
		DIN_66003 = 24,
		NS_4551_1 = 25,
		NF_Z_62_010 = 26,
		ISO_10646_UTF_1 = 27,
		ISO_646_basic = 28,
		INVARIANT = 29,
		ISO_646_irv = 30,
		NATS_SEFI = 31,
		NATS_SEFI_ADD = 32,
		NATS_DANO = 33,
		NATS_DANO_ADD = 34,
		SEN_850200_B = 35,
		KS_C_5601 = 36,
		ISO_2022_KR = 37,
		EUC_KR = 38,
		ISO_2022_JP = 39,
		ISO_2022_JP_2 = 40,
		JIS_C6220_jp = 41,
		JIS_C6220_ro = 42,
		PT = 43,
		greek7_old = 44,
		latin_greek = 45,
		NF_Z_62_010 = 46,
		Latin_greek_1 = 47,
		ISO_5427 = 48,
		JIS_C6226= 49,
		BS_viewdata = 50,
		INIS = 51,
		INIS_8 = 52,
		INIS_cyrillic = 53,
		ISO_5427 = 54,
		ISO_5428 = 55,
		GB_1988_80 = 56,
		NS_4551_2 = 58,
		videotex_suppl = 59,
		PT2 = 60,
		ES2 = 61,
		MSZ_7795_3 = 62,
		JIS_C6226 = 63,
		greek7 = 64,
		ASMO_449 = 65,
		iso_ir_90 = 66,
		JIS_C6229_a = 67,
		JIS_C6229_b = 68,
		JIS_C6229_b_add = 69,
		JIS_C6229_hand = 70,
		JIS_C6229_hand_add = 71,
		JIS_C6229_kana = 72,
		ISO_2033 = 73,
		ANSI_X3_110 = 74,	// US ASCII for Western Union teleprinters.
		T_61_7bit = 75,
		T_61_8bit = 76,
		ECMA_cyrillic = 77,
		CSA_Z243_4 = 78,
		CSA_Z243_4_2 = 79,
		CSA_Z243_4_gr = 80,
		ISO_8859_6_E = 81,
		ISO_8859_6_I = 82,
		T_101_G2 = 83,
		ISO_8859_8_E = 84,
		ISO_8859_8_I = 85,
		CSN_369103 = 86,
		JUS_I_B1_002 = 87,
		IEC_P27_1 = 88,
		JUS_I_B1_003_serb = 89,
		JUS_I_B1_003_mac = 90 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	macedonian
iso-ir-147
csISO147Macedonian
	greek-ccitt 	91 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	iso-ir-150
csISO150
csISO150GreekCCITT
	NC_NC00-10:81 	92 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	cuba
iso-ir-151
ISO646-CU
csISO151Cuba
	ISO_6937-2-25 	93 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	iso-ir-152
csISO6937Add
	GOST_19768-74 	94 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	ST_SEV_358-88
iso-ir-153
csISO153GOST1976874
	ISO_8859-supp 	95 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	iso-ir-154
latin1-2-5
csISO8859Supp
	ISO_10367-box 	96 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	iso-ir-155
csISO10367Box
	latin-lap 	97 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	lap
iso-ir-158
csISO158Lap
	JIS_X0212-1990 	98 	[ISO-IR: International Register of Escape Sequences]
Note: The current registration authority is IPSJ/ITSCJ, Japan. 	[RFC1345][Keld_Simonsen] 	x0212
iso-ir-159
csISO159JISX02121990
	DS_2089 	99 	Danish Standard, DS 2089, February 1974 	[RFC1345][Keld_Simonsen] 	DS2089
ISO646-DK
dk
csISO646Danish
	us-dk 	100 		[RFC1345][Keld_Simonsen] 	csUSDK
	dk-us 	101 		[RFC1345][Keld_Simonsen] 	csDKUS
	KSC5636 	102 		[RFC1345][Keld_Simonsen] 	ISO646-KR
csKSC5636
	UNICODE-1-1-UTF-7 	103 	[RFC1642] 	[RFC1642] 	csUnicode11UTF7
	ISO-2022-CN 	104 	[RFC1922] 	[RFC1922] 	csISO2022CN
	ISO-2022-CN-EXT 	105 	[RFC1922] 	[RFC1922] 	csISO2022CNEXT
	UTF-8 	106 	[RFC3629] 	[RFC3629] 	csUTF8
	ISO-8859-13 	109 	ISO See [http://www.iana.org/assignments/charset-reg/ISO-8859-13][Vladas_Tumasonis] 		csISO885913
	ISO-8859-14 	110 	ISO See [http://www.iana.org/assignments/charset-reg/ISO-8859-14] [Keld_Simonsen_2] 		iso-ir-199
ISO_8859-14:1998
ISO_8859-14
latin8
iso-celtic
l8
csISO885914
	ISO-8859-15 	111 	ISO Please see: [http://www.iana.org/assignments/charset-reg/ISO-8859-15] 		ISO_8859-15
Latin-9
csISO885915
	ISO-8859-16 	112 	ISO 		iso-ir-226
ISO_8859-16:2001
ISO_8859-16
latin10
l10
csISO885916
	GBK 	113 	Chinese IT Standardization Technical Committee Please see: [http://www.iana.org/assignments/charset-reg/GBK] 		CP936
MS936
windows-936
csGBK
	GB18030 	114 	Chinese IT Standardization Technical Committee Please see: [http://www.iana.org/assignments/charset-reg/GB18030] 		csGB18030
	OSD_EBCDIC_DF04_15 	115 	Fujitsu-Siemens standard mainframe EBCDIC encoding Please see: [http://www.iana.org/assignments/charset-reg/OSD-EBCDIC-DF04-15] 		csOSDEBCDICDF0415
	OSD_EBCDIC_DF03_IRV 	116 	Fujitsu-Siemens standard mainframe EBCDIC encoding Please see: [http://www.iana.org/assignments/charset-reg/OSD-EBCDIC-DF03-IRV] 		csOSDEBCDICDF03IRV
	OSD_EBCDIC_DF04_1 	117 	Fujitsu-Siemens standard mainframe EBCDIC encoding Please see: [http://www.iana.org/assignments/charset-reg/OSD-EBCDIC-DF04-1] 		csOSDEBCDICDF041
	ISO-11548-1 	118 	See [http://www.iana.org/assignments/charset-reg/ISO-11548-1] [Samuel_Thibault] 		ISO_11548-1
ISO_TR_11548-1
csISO115481
	KZ-1048 	119 	See [http://www.iana.org/assignments/charset-reg/KZ-1048] [Sairan_M_Kikkarin][Alexei_Veremeev] 		STRK1048-2002
RK1048
csKZ1048
	ISO-10646-UCS-2 	1000 	the 2-octet Basic Multilingual Plane, aka Unicode this needs to specify network byte order: the standard does not specify (it is a 16-bit integer space) 		csUnicode
	ISO-10646-UCS-4 	1001 	the full code space. (same comment about byte order, these are 31-bit numbers. 		csUCS4
	ISO-10646-UCS-Basic 	1002 	ASCII subset of Unicode. Basic Latin = collection 1 See ISO 10646, Appendix A 		csUnicodeASCII
	ISO-10646-Unicode-Latin1 	1003 	ISO Latin-1 subset of Unicode. Basic Latin and Latin-1 Supplement = collections 1 and 2. See ISO 10646, Appendix A. See [RFC1815]. 		csUnicodeLatin1
ISO-10646
	ISO-10646-J-1 	1004 	ISO 10646 Japanese, see [RFC1815]. 		csUnicodeJapanese
	ISO-Unicode-IBM-1261 	1005 	IBM Latin-2, -3, -5, Extended Presentation Set, GCSGID: 1261 		csUnicodeIBM1261
	ISO-Unicode-IBM-1268 	1006 	IBM Latin-4 Extended Presentation Set, GCSGID: 1268 		csUnicodeIBM1268
	ISO-Unicode-IBM-1276 	1007 	IBM Cyrillic Greek Extended Presentation Set, GCSGID: 1276 		csUnicodeIBM1276
	ISO-Unicode-IBM-1264 	1008 	IBM Arabic Presentation Set, GCSGID: 1264 		csUnicodeIBM1264
	ISO-Unicode-IBM-1265 	1009 	IBM Hebrew Presentation Set, GCSGID: 1265 		csUnicodeIBM1265
	UNICODE-1-1 	1010 	[RFC1641] 	[RFC1641] 	csUnicode11
	SCSU 	1011 	SCSU See [http://www.iana.org/assignments/charset-reg/SCSU] [Markus_Scherer] 		csSCSU
	UTF-7 	1012 	[RFC2152] 	[RFC2152] 	csUTF7
	UTF-16BE 	1013 	[RFC2781] 	[RFC2781] 	csUTF16BE
	UTF-16LE 	1014 	[RFC2781] 	[RFC2781] 	csUTF16LE
	UTF-16 	1015 	[RFC2781] 	[RFC2781] 	csUTF16
	CESU-8 	1016 	[http://www.unicode.org/unicode/reports/tr26] 	[Toby_Phipps] 	csCESU8
csCESU-8
	UTF-32 	1017 	[http://www.unicode.org/unicode/reports/tr19/] 	[Mark_Davis] 	csUTF32
	UTF-32BE 	1018 	[http://www.unicode.org/unicode/reports/tr19/] 	[Mark_Davis] 	csUTF32BE
	UTF-32LE 	1019 	[http://www.unicode.org/unicode/reports/tr19/] 	[Mark_Davis] 	csUTF32LE
	BOCU-1 	1020 	[http://www.unicode.org/notes/tn6/] 	[Markus_Scherer] 	csBOCU1
csBOCU-1
	ISO-8859-1-Windows-3.0-Latin-1 	2000 	Extended ISO 8859-1 Latin-1 for Windows 3.0. PCL Symbol Set id: 9U 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csWindows30Latin1
	ISO-8859-1-Windows-3.1-Latin-1 	2001 	Extended ISO 8859-1 Latin-1 for Windows 3.1. PCL Symbol Set id: 19U 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csWindows31Latin1
	ISO-8859-2-Windows-Latin-2 	2002 	Extended ISO 8859-2. Latin-2 for Windows 3.1. PCL Symbol Set id: 9E 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csWindows31Latin2
	ISO-8859-9-Windows-Latin-5 	2003 	Extended ISO 8859-9. Latin-5 for Windows 3.1 PCL Symbol Set id: 5T 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csWindows31Latin5
	hp-roman8 	2004 	LaserJet IIP Printer User's Manual, HP part no 33471-90901, Hewlet-Packard, June 1989. 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.][RFC1345][Keld_Simonsen] 	roman8
r8
csHPRoman8
	Adobe-Standard-Encoding 	2005 	PostScript Language Reference Manual PCL Symbol Set id: 10J 	[Adobe Systems Incorporated, PostScript Language Reference Manual, second edition, Addison-Wesley Publishing Company, Inc., 1990.] 	csAdobeStandardEncoding
	Ventura-US 	2006 	Ventura US. ASCII plus characters typically used in publishing, like pilcrow, copyright, registered, trade mark, section, dagger, and double dagger in the range A0 (hex) to FF (hex). PCL Symbol Set id: 14J 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csVenturaUS
	Ventura-International 	2007 	Ventura International. ASCII plus coded characters similar to Roman8. PCL Symbol Set id: 13J 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csVenturaInternational
	DEC-MCS 	2008 	VAX/VMS User's Manual, Order Number: AI-Y517A-TE, April 1986. 	[RFC1345][Keld_Simonsen] 	dec
csDECMCS
	IBM850 	2009 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp850
850
csPC850Multilingual
	PC8-Danish-Norwegian 	2012 	PC Danish Norwegian 8-bit PC set for Danish Norwegian PCL Symbol Set id: 11U 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csPC8DanishNorwegian
	IBM862 	2013 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp862
862
csPC862LatinHebrew
	PC8-Turkish 	2014 	PC Latin Turkish. PCL Symbol Set id: 9T 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csPC8Turkish
	IBM-Symbols 	2015 	Presentation Set, CPGID: 259 	[IBM Corporation, "ABOUT TYPE: IBM's Technical Reference for Core Interchange Digitized Type", Publication number S544-3708-01] 	csIBMSymbols
	IBM-Thai 	2016 	Presentation Set, CPGID: 838 	[IBM Corporation, "ABOUT TYPE: IBM's Technical Reference for Core Interchange Digitized Type", Publication number S544-3708-01] 	csIBMThai
	HP-Legal 	2017 	PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 1U 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csHPLegal
	HP-Pi-font 	2018 	PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 15U 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csHPPiFont
	HP-Math8 	2019 	PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 8M 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csHPMath8
	Adobe-Symbol-Encoding 	2020 	PostScript Language Reference Manual PCL Symbol Set id: 5M 	[Adobe Systems Incorporated, PostScript Language Reference Manual, second edition, Addison-Wesley Publishing Company, Inc., 1990.] 	csHPPSMath
	HP-DeskTop 	2021 	PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 7J 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csHPDesktop
	Ventura-Math 	2022 	PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 6M 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csVenturaMath
	Microsoft-Publishing 	2023 	PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 6J 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	csMicrosoftPublishing
	Windows-31J 	2024 	Windows Japanese. A further extension of Shift_JIS to include NEC special characters (Row 13), NEC selection of IBM extensions (Rows 89 to 92), and IBM extensions (Rows 115 to 119). The CCS's are JIS X0201:1997, JIS X0208:1997, and these extensions. This charset can be used for the top-level media type "text", but it is of limited or specialized use (see [RFC2278]). PCL Symbol Set id: 19K 		csWindows31J
GB2312 	GB2312 	2025 	Chinese for People's Republic of China (PRC) mixed one byte, two byte set: 20-7E = one byte ASCII A1-FE = two byte PRC Kanji See GB 2312-80 PCL Symbol Set Id: 18C 		csGB2312
Big5 	Big5 	2026 	Chinese for Taiwan Multi-byte set. PCL Symbol Set Id: 18T 		csBig5
	macintosh 	2027 	The Unicode Standard ver1.0, ISBN 0-201-56788-1, Oct 1991 	[RFC1345][Keld_Simonsen] 	mac
csMacintosh
	IBM037 	2028 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp037
ebcdic-cp-us
ebcdic-cp-ca
ebcdic-cp-wt
ebcdic-cp-nl
csIBM037
	IBM038 	2029 	IBM 3174 Character Set Ref, GA27-3831-02, March 1990 	[RFC1345][Keld_Simonsen] 	EBCDIC-INT
cp038
csIBM038
	IBM273 	2030 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP273
csIBM273
	IBM274 	2031 	IBM 3174 Character Set Ref, GA27-3831-02, March 1990 	[RFC1345][Keld_Simonsen] 	EBCDIC-BE
CP274
csIBM274
	IBM275 	2032 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	EBCDIC-BR
cp275
csIBM275
	IBM277 	2033 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	EBCDIC-CP-DK
EBCDIC-CP-NO
csIBM277
	IBM278 	2034 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP278
ebcdic-cp-fi
ebcdic-cp-se
csIBM278
	IBM280 	2035 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP280
ebcdic-cp-it
csIBM280
	IBM281 	2036 	IBM 3174 Character Set Ref, GA27-3831-02, March 1990 	[RFC1345][Keld_Simonsen] 	EBCDIC-JP-E
cp281
csIBM281
	IBM284 	2037 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP284
ebcdic-cp-es
csIBM284
	IBM285 	2038 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP285
ebcdic-cp-gb
csIBM285
	IBM290 	2039 	IBM 3174 Character Set Ref, GA27-3831-02, March 1990 	[RFC1345][Keld_Simonsen] 	cp290
EBCDIC-JP-kana
csIBM290
	IBM297 	2040 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp297
ebcdic-cp-fr
csIBM297
	IBM420 	2041 	IBM NLS RM Vol2 SE09-8002-01, March 1990, IBM NLS RM p 11-11 	[RFC1345][Keld_Simonsen] 	cp420
ebcdic-cp-ar1
csIBM420
	IBM423 	2042 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp423
ebcdic-cp-gr
csIBM423
	IBM424 	2043 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp424
ebcdic-cp-he
csIBM424
	IBM437 	2011 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp437
437
csPC8CodePage437
	IBM500 	2044 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP500
ebcdic-cp-be
ebcdic-cp-ch
csIBM500
	IBM851 	2045 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp851
851
csIBM851
	IBM852 	2010 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp852
852
csPCp852
	IBM855 	2046 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp855
855
csIBM855
	IBM857 	2047 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp857
857
csIBM857
	IBM860 	2048 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp860
860
csIBM860
	IBM861 	2049 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp861
861
cp-is
csIBM861
	IBM863 	2050 	IBM Keyboard layouts and code pages, PN 07G4586 June 1991 	[RFC1345][Keld_Simonsen] 	cp863
863
csIBM863
	IBM864 	2051 	IBM Keyboard layouts and code pages, PN 07G4586 June 1991 	[RFC1345][Keld_Simonsen] 	cp864
csIBM864
	IBM865 	2052 	IBM DOS 3.3 Ref (Abridged), 94X9575 (Feb 1987) 	[RFC1345][Keld_Simonsen] 	cp865
865
csIBM865
	IBM868 	2053 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP868
cp-ar
csIBM868
	IBM869 	2054 	IBM Keyboard layouts and code pages, PN 07G4586 June 1991 	[RFC1345][Keld_Simonsen] 	cp869
869
cp-gr
csIBM869
	IBM870 	2055 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP870
ebcdic-cp-roece
ebcdic-cp-yu
csIBM870
	IBM871 	2056 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP871
ebcdic-cp-is
csIBM871
	IBM880 	2057 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp880
EBCDIC-Cyrillic
csIBM880
	IBM891 	2058 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp891
csIBM891
	IBM903 	2059 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp903
csIBM903
	IBM904 	2060 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	cp904
904
csIBBM904
	IBM905 	2061 	IBM 3174 Character Set Ref, GA27-3831-02, March 1990 	[RFC1345][Keld_Simonsen] 	CP905
ebcdic-cp-tr
csIBM905
	IBM918 	2062 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP918
ebcdic-cp-ar2
csIBM918
	IBM1026 	2063 	IBM NLS RM Vol2 SE09-8002-01, March 1990 	[RFC1345][Keld_Simonsen] 	CP1026
csIBM1026
	EBCDIC-AT-DE 	2064 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csIBMEBCDICATDE
	EBCDIC-AT-DE-A 	2065 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICATDEA
	EBCDIC-CA-FR 	2066 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICCAFR
	EBCDIC-DK-NO 	2067 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICDKNO
	EBCDIC-DK-NO-A 	2068 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICDKNOA
	EBCDIC-FI-SE 	2069 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICFISE
	EBCDIC-FI-SE-A 	2070 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICFISEA
	EBCDIC-FR 	2071 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICFR
	EBCDIC-IT 	2072 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICIT
	EBCDIC-PT 	2073 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICPT
	EBCDIC-ES 	2074 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICES
	EBCDIC-ES-A 	2075 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICESA
	EBCDIC-ES-S 	2076 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICESS
	EBCDIC-UK 	2077 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICUK
	EBCDIC-US 	2078 	IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987 	[RFC1345][Keld_Simonsen] 	csEBCDICUS
	UNKNOWN-8BIT 	2079 		[RFC1428] 	csUnknown8BiT
	MNEMONIC 	2080 	[RFC1345], also known as "mnemonic+ascii+38" 	[RFC1345][Keld_Simonsen] 	csMnemonic
	MNEM 	2081 	[RFC1345], also known as "mnemonic+ascii+8200" 	[RFC1345][Keld_Simonsen] 	csMnem
	VISCII 	2082 	[RFC1456] 	[RFC1456] 	csVISCII
	VIQR 	2083 	[RFC1456] 	[RFC1456] 	csVIQR
KOI8-R 	KOI8-R 	2084 	[RFC1489], based on GOST-19768-74, ISO-6937/8, INIS-Cyrillic, ISO-5427. 	[RFC1489] 	csKOI8R
	HZ-GB-2312 	2085 	[RFC1842], [RFC1843][RFC1843][RFC1842]
	IBM866 	2086 	IBM NLDG Volume 2 (SE09-8002-03) August 1994 	[Rick_Pond] 	cp866
866
csIBM866
	IBM775 	2087 	HP PCL 5 Comparison Guide (P/N 5021-0329) pp B-13, 1996 	[Hewlett-Packard Company, "HP PCL 5 Comparison Guide", (P/N 5021-0329) pp B-13, 1996.] 	cp775
csPC775Baltic
	KOI8-U 	2088 	[RFC2319] 	[RFC2319] 	csKOI8U
	IBM00858 	2089 	IBM See [http://www.iana.org/assignments/charset-reg/IBM00858] [Tamer_Mahdi] 		CCSID00858
CP00858
PC-Multilingual-850+euro
csIBM00858
	IBM00924 	2090 	IBM See [http://www.iana.org/assignments/charset-reg/IBM00924] [Tamer_Mahdi] 		CCSID00924
CP00924
ebcdic-Latin9--euro
csIBM00924
	IBM01140 	2091 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01140] [Tamer_Mahdi] 		CCSID01140
CP01140
ebcdic-us-37+euro
csIBM01140
	IBM01141 	2092 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01141] [Tamer_Mahdi] 		CCSID01141
CP01141
ebcdic-de-273+euro
csIBM01141
	IBM01142 	2093 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01142] [Tamer_Mahdi] 		CCSID01142
CP01142
ebcdic-dk-277+euro
ebcdic-no-277+euro
csIBM01142
	IBM01143 	2094 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01143] [Tamer_Mahdi] 		CCSID01143
CP01143
ebcdic-fi-278+euro
ebcdic-se-278+euro
csIBM01143
	IBM01144 	2095 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01144] [Tamer_Mahdi] 		CCSID01144
CP01144
ebcdic-it-280+euro
csIBM01144
	IBM01145 	2096 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01145] [Tamer_Mahdi] 		CCSID01145
CP01145
ebcdic-es-284+euro
csIBM01145
	IBM01146 	2097 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01146] [Tamer_Mahdi] 		CCSID01146
CP01146
ebcdic-gb-285+euro
csIBM01146
	IBM01147 	2098 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01147] [Tamer_Mahdi] 		CCSID01147
CP01147
ebcdic-fr-297+euro
csIBM01147
	IBM01148 	2099 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01148] [Tamer_Mahdi] 		CCSID01148
CP01148
ebcdic-international-500+euro
csIBM01148
	IBM01149 	2100 	IBM See [http://www.iana.org/assignments/charset-reg/IBM01149] [Tamer_Mahdi] 		CCSID01149
CP01149
ebcdic-is-871+euro
csIBM01149
	Big5-HKSCS 	2101 	See [http://www.iana.org/assignments/charset-reg/Big5-HKSCS] 	[Nicky_Yick] 	csBig5HKSCS
	IBM1047 	2102 	IBM1047 (EBCDIC Latin 1/Open Systems) [http://www-1.ibm.com/servers/eserver/iseries/software/globalization/pdf/cp01047z.pdf] 	[Reuel_Robrigado] 	IBM-1047
csIBM1047
	PTCP154 	2103 	See [http://www.iana.org/assignments/charset-reg/PTCP154] 	[Alexander_Uskov] 	csPTCP154
PT154
CP154
Cyrillic-Asian
	Amiga-1251 	2104 	See [http://www.amiga.ultranet.ru/Amiga-1251.html] 		Ami1251
Amiga1251
Ami-1251
csAmiga1251 (Aliases are provided for historical reasons and should not be used) [Malyshev]
	KOI7-switched 	2105 	See [http://www.iana.org/assignments/charset-reg/KOI7-switched] 		csKOI7switched
	BRF 	2106 	See [http://www.iana.org/assignments/charset-reg/BRF] [Samuel_Thibault] 		csBRF
	TSCII 	2107 	See [http://www.iana.org/assignments/charset-reg/TSCII] [Kuppuswamy_Kalyanasu] 		csTSCII
	CP51932 	2108 	See [http://www.iana.org/assignments/charset-reg/CP51932] [Yui_Naruse] 		csCP51932
	windows-874 	2109 	See [http://www.iana.org/assignments/charset-reg/windows-874] [Shawn_Steele] 		cswindows874
	windows-1250 	2250 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1250] [Katya_Lazhintseva] 		cswindows1250
	windows-1251 	2251 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1251] [Katya_Lazhintseva] 		cswindows1251
	windows-1252 	2252 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1252] [Chris_Wendt] 		cswindows1252
	windows-1253 	2253 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1253] [Katya_Lazhintseva] 		cswindows1253
	windows-1254 	2254 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1254] [Katya_Lazhintseva] 		cswindows1254
	windows-1255 	2255 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1255] [Katya_Lazhintseva] 		cswindows1255
	windows-1256 	2256 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1256] [Katya_Lazhintseva] 		cswindows1256
	windows-1257 	2257 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1257] [Katya_Lazhintseva] 		cswindows1257
	windows-1258 	2258 	Microsoft [http://www.iana.org/assignments/charset-reg/windows-1258] [Katya_Lazhintseva] 		cswindows1258
	TIS-620 	2259 	Thai Industrial Standards Institute (TISI) [Trin_Tantsetthi] 		csTIS620
ISO-8859-11
	CP50220 	2260 	See [http://www.iana.org/assignments/charset-reg/CP50220] [Yui_Naruse] 		csCP50220

	CPConverter();
};

#endif // CPCONVERTER_H
