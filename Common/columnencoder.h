//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef COLUMNENCODER_H
#define COLUMNENCODER_H

#include <string>
#include <vector>
#include <map>
#include <set>
#include "columntype.h"
#ifdef BUILDING_JASP
#include <json/json.h>
#else
#include "json/json.h"
#endif

/// Class to "encode" the names of columns
/// It can be used both directly, through columnEncoder()->, in that scenario it only en- and decodes actual columnNames from the dataset.
/// If you want to en- or decode other names then you instantiate a separate copy and use it's functions.
/// It will then also use the columnnames if they are set btw.
class DataSet;
class ColumnEncoder
{
	friend class DataSet;
public:
	typedef std::map<std::string, std::string>					colMap;
	typedef std::map<std::string, columnType>					colTypeMap;	
	typedef std::vector<std::string>							colVec;
	typedef std::set<ColumnEncoder *>							ColumnEncoders;
	typedef std::set<std::pair<std::string, columnType>>		colsPlusTypes;

private:						ColumnEncoder() { invalidateAll(); }
public:
								ColumnEncoder(std::string prefix, std::string postfix = "_Encoded");
								ColumnEncoder(const std::map<std::string, std::string> & decodeDifferently);
								~ColumnEncoder();
	static ColumnEncoder	*	columnEncoder();
	/// Like columnEncoder() but never consults/repopulates the process-global current-encoder
	/// indirection (_currentEncoder). Used by the desktop so it never depends on the global,
	/// which is only meaningful in the engine's request context.
	static ColumnEncoder	*	fallbackEncoder();
	static ColumnEncoder	*	currentEncoder();
	static void					setCurrentEncoder(ColumnEncoder * encoder);

	static	bool				isColumnName(const std::string & in)							{ return columnEncoder()->shouldEncode(in); }
	static	bool				isEncodedColumnName(const std::string & in)						{ return columnEncoder()->shouldDecode(in); }
	static	void				setCurrentColumnNames(const colTypeMap & names)					{ columnEncoder()->setCurrentNames(names);	}
	static	void				updateColumnTypesOnlyStatic(const colTypeMap & names)			{ columnEncoder()->updateColumnTypesOnly(names);	}

	//backwards compat:
	static	void				setCurrentColumnNames(const std::vector<std::string> & names)	{ columnEncoder()->setCurrentNames(names);	}
	static	void				setCurrentColTypePerName(const colTypeMap & theMap)				{ columnEncoder()->setCurrentColumnTypePerName(theMap);	}

	static	std::string			replaceColumnNamesInRScript(const std::string & rCode, const std::map<std::string, std::string> & changedNames);
	static	std::string			removeColumnNamesFromRScript(const std::string & rCode, const std::vector<std::string> & colsToRemove);
	
	static	colVec				columnNames();
	static	colVec				columnNamesEncoded();

			bool				shouldEncode(const std::string & in);
			bool				shouldDecode(const std::string & in);
			void				setCurrentNames(const colTypeMap & names);
			const colTypeMap&	currentNames() const { return _dataSetTypes; }
			void				updateColumnTypesOnly(const colTypeMap & names);
			void				setCurrentNames(const std::vector<std::string> & names, bool generateTypesEncoding=true);	///< Do not use! Deprecated
			void				setCurrentColumnTypePerName(const colTypeMap & theMap);									///< Do not use! Deprecated
			void				setCurrentNamesFromOptionsMeta(const Json::Value & json);

			std::string			encode(const std::string &in);
			std::string			decode(const std::string &in);

			columnType			columnTypeFromEncoded(const std::string & in);


			///Replace all occurences of columnNames in a string by their encoded versions, taking into account the presence of word boundaries and parentheses.
			std::string			encodeRScript(std::string text, std::set<std::string> * columnNamesFound = nullptr);
			std::string			encodeRScript(std::string text, std::map<std::string, std::set<std::string>>& prefixedColumnsFound, const std::set<std::string>& allowedPrefixes);
			std::string			encodeRScript(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, std::set<std::string> * columnNamesFound = nullptr, const std::string& acceptedPrefix = "");

		///Replace all occurences of columnNames in a string by their encoded versions, regardless of word boundaries or parentheses.
		///Instance method: en/decode against *this* encoder. Engine callers use the process-global
		///current encoder (columnEncoder()) and desktop callers the dataset's own encoder.
			std::string			encodeAll(const std::string & text) const;

			///Replace all occurences of encoded columnNames in a string by their decoded versions, regardless of word boundaries or parentheses.
			std::string			decodeAll(const std::string & text) const;

			///Replace all occurences of columnNames in a string by their encoded versions in all json-names and string-values, regardless of word boundaries or parentheses.
			void				encodeJson(Json::Value & json, bool replaceNames = false, bool replaceStrict = false) const;

			///Replace all occurences of encoded columnNames in a string by their decoded versions in all json-names and string-values, regardless of word boundaries or parentheses.
			void				decodeJson(Json::Value & json, bool replaceNames = true) const;

	static	void				decodeJsonSafeHtml(Json::Value & json);

	static	colsPlusTypes		encodeColumnNamesinOptions(Json::Value & options, bool preloadingData);

private:
	static	void				_convertPreloadingDataOption(Json::Value & option, const std::string& optionName, colsPlusTypes& colTypes);
	static	void				_addTypeToColumnNamesInOptionsRecursively(Json::Value & options, bool preloadingData, colsPlusTypes& colTypes);
	static	void				_encodeColumnNamesinOptions(Json::Value & options, Json::Value & meta);

private:
	static	std::string			replaceAll(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names);
	static  std::string			replaceAllStrict(const std::string & text, const std::map<std::string, std::string> & map);

	static	void				replaceAll(Json::Value & json, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, bool replaceNames, bool replaceStrict);
	static	std::vector<size_t>	getPositionsColumnNameMatches(const std::string & text, const std::string & columnName);
			void				collectExtraEncodingsFromMetaJson(const Json::Value & in, colTypeMap & namesCollected) const;
	static	void				sortVectorBigToSmall(std::vector<std::string> & vec);
	static	const colMap	&	encodingMap();
	static	const colMap	&	decodingMap();
	static	const colTypeMap&	decodingTypes();
	static	const colMap	&	decodingMapSafeHtml();
	static	const colVec	&	originalNames();
	static	const colVec	&	encodedNames();
	//Merged-map builders rooted at a specific encoder (used by the instance en/decode helpers, which
	//must include the extra encodings from _otherEncoders exactly like the process-global getters).
	static	colMap				encodingMap(const ColumnEncoder * self);
	static	colMap				decodingMap(const ColumnEncoder * self);
	static	colVec				originalNames(const ColumnEncoder * self);
	static	colVec				encodedNames(const ColumnEncoder * self);
	static	void				invalidateAll();

	static	bool				_encodingMapInvalidated,
								_decodingMapInvalidated,
								_decodingTypeInvalidated,
								_decoSafeMapInvalidated,
								_originalNamesInvalidated,
								_encodedNamesInvalidated;

	static ColumnEncoder	*	_columnEncoder;
	static ColumnEncoder	*	_currentEncoder;
	static ColumnEncoders	*	_otherEncoders;

	colMap						_encodingMap,
								_decodingMap;
	colVec						_originalNames,
								_encodedNames;
	colTypeMap					_decodingTypes,
								_dataSetTypes;

	std::string					_encodePrefix  = "JaspColumn_",
								_encodePostfix = "_Encoded";
};

#endif // COLUMNENCODER_H
