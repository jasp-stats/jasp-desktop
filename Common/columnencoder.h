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
#include <functional>
#include "columntype.h"
#ifdef BUILDING_JASP
#include <json/json.h>
#else
#include "json/json.h"
#endif

/// Class to "encode" the names of columns. It runs in three modes (mds-plan Phase 1, C1-C7):
///
///  * FULL      - self-contained instance: owns a mint (name -> index and index -> (original,
///                type)) AND materializes the full-string encode/decode maps in its own prefix
///                namespace. Its namespace is registered as a block in the process-global strict
///                decode table (see the static decode()). Used by the process-global fallback encoder,
///                the extras encoder (DataBridge's `JaspExtraOptions_`, C6) and ad-hoc callers
///                (tests, ColumnEncoderContext replay).
///
///  * CANONICAL - the per-DataSet mint (C2), created via createCanonical(): keeps only
///                `name -> index` and `index -> (originalName, type)` built by
///                setCurrentNames()/updateColumnTypesOnly() with the classic alphabetical-counter
///                semantics (`.scale`/`.ordinal`/`.nominal` triples + bare-name default). It
///                NEVER holds or answers full-string encode/decode; instead every live VIEW
///                minted on top of it gets its block rewritten whenever the mint changes.
///
///  * VIEW      - the per-Filter namespace handle (C3), created via createView(): a non-owning
///                {canonical*, prefix} pair. encode() = canonical index lookup + own prefix
///                compose, shouldEncode() = canonical membership, no materialized per-view
///                string maps. Its block lives in the global decode table and is erased when
///                the view dies (deleted filter's tokens go raw, C16).
///
/// Decoding is always context-free (C4): the ONE process-global table (a block per live filter
/// of every loaded dataset plus the extras and fallback blocks) is the only decode source,
/// membership-only, unknown tokens stay raw. Encode-side unioning of the registered "other"
/// encoders (extras) survives per C5; all decode-side merging is gone.
///
/// Per-process ownership: the desktop, the engine and the syntax-bridge each keep their own
/// canonicals/views/table - they are separate processes. Deterministic minting (identical
/// column maps + ids on the shared sqlite db) makes them mint identical blocks.
class DataSet;
class ColumnEncoder
{
	friend class DataSet;

	enum class Mode { full, canonical, view };
	struct CanonicalTag {};

public:
	typedef std::map<std::string, std::string>					colMap;
	typedef std::map<std::string, columnType>					colTypeMap;	
	typedef std::vector<std::string>							colVec;
	typedef std::set<ColumnEncoder *>							ColumnEncoders;
	typedef std::set<std::pair<std::string, columnType>>		colsPlusTypes;
	typedef std::map<int, colsPlusTypes>						perDataSetColsPlusTypes;
	///< Resolves the encoder to use for a given option stamp (dataSetId, filterId), or nullptr to
	/// fall back to the current encoder. filterId -1 means "no filter stamped" (unqualified option:
	/// the resolver should return the dataset's shown-filter view / legacy fallback, C7).
	typedef std::function<ColumnEncoder *(int dataSetId, int filterId)>	EncoderFor;

	/// One entry of the process-global strict decode table (C4): token -> (original name, type).
	struct DecodeEntry
	{
		std::string	name	= "";
		columnType	type	= columnType::unknown;
	};

private:						ColumnEncoder() { /* FULL, default prefix */ }
public:
								ColumnEncoder(std::string prefix, std::string postfix = "_Encoded");
								ColumnEncoder(const std::map<std::string, std::string> & decodeDifferently);
								~ColumnEncoder();

	/// Create the canonical mint for a DataSet (C2): mint-only, answers no full-string
	/// encode/decode, registers no block itself (its views' blocks are its footprint).
	static ColumnEncoder	*	createCanonical();
	/// Create a view (C3) on `canonical` minting `prefix + <index> + postfix` tokens; registers
	/// with the canonical and mints its block in the global decode table right away. Destroying
	/// the view (or the canonical) erases the block again.
	static ColumnEncoder	*	createView(ColumnEncoder * canonical, const std::string & prefix, const std::string & postfix = "_Encoded");

	static ColumnEncoder	*	columnEncoder();
	/// Like columnEncoder() but never consults/repopulates the process-global current-encoder
	/// indirection (_currentEncoder). Used by the desktop so it never depends on the global,
	/// which is only meaningful in the engine's request context.
	static ColumnEncoder	*	fallbackEncoder();
	static ColumnEncoder	*	currentEncoder();
	/// Points the legacy non-aware request-context indirection at `encoder` (C7: after Phase 1
	/// this is a VIEW - the shown dataset's shown filter's handle - never a canonical).
	static void					setCurrentEncoder(ColumnEncoder * encoder);

	static	bool				isColumnName(const std::string & in)							{ return columnEncoder()->shouldEncode(in);	}
	static	bool				isEncodedColumnName(const std::string & in)						{ return shouldDecode(in);					}
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

			/// Mint a fresh namespace. On the canonical and on FULL instances this (re)builds the
			/// mint and rewrites every dependent block; on a VIEW it funnels into its canonical
			/// (the legacy ColumnEncoderContext replay path mutates the dataset mint through the
			/// current view, exactly like it used to mutate the dataset's own encoder).
			void				setCurrentNames(const colTypeMap & names);
			const colTypeMap&	currentNames() const											{ return _mode == Mode::view && _canonical ? _canonical->_dataSetTypes : _dataSetTypes;	}
			///< The block prefix of this instance ("" on a canonical, which has no namespace).
			std::string			encodePrefix() const											{ return _encodePrefix;																				}
			///< Reforge this instance's namespace under a different prefix, regenerating the
			///< encoded names from the current ones. Full/view only (the canonical has no
			///< prefix of its own). Used to replay a captured ColumnEncoderContext whose dataset
			///< carried its own prefix.
			void				setEncodePrefix(const std::string & prefix);
			void				updateColumnTypesOnly(const colTypeMap & names);
			void				setCurrentNames(const std::vector<std::string> & names, bool generateTypesEncoding=true);	///< Do not use! Deprecated
			void				setCurrentColumnTypePerName(const colTypeMap & theMap);									///< Do not use! Deprecated
			void				setCurrentNamesFromOptionsMeta(const Json::Value & json);

			/// raw name (bare or `name.type`) -> full encoded token. Canonical never answers
			/// (C2); a view composes canonical index + own prefix (C3).
			std::string			encode(const std::string &in);

			///Replace all occurences of columnNames in a string by their encoded versions, taking into account the presence of word boundaries and parentheses.
			std::string			encodeRScript(std::string text, std::set<std::string> * columnNamesFound = nullptr);
			std::string			encodeRScript(std::string text, std::map<std::string, std::set<std::string>>& prefixedColumnsFound, const std::set<std::string>& allowedPrefixes);
			std::string			encodeRScript(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, std::set<std::string> * columnNamesFound = nullptr, const std::string& acceptedPrefix = "");

		///Replace all occurences of columnNames in a string by their encoded versions, regardless of word boundaries or parentheses.
		///Instance method: encodes against *this* encoder (own namespace + the registered extras,
		///C5). Decode is context-free and goes through the global table instead.
			std::string			encodeAll(const std::string & text) const;

			///Replace all occurences of columnNames in a string by their encoded versions in all json-names and string-values, regardless of word boundaries or parentheses.
			void				encodeJson(Json::Value & json, bool replaceNames = false, bool replaceStrict = false) const;

	/// --- The ONE process-global strict decode table (C4) -------------------------------
	/// Single writer: rebuildBlock() (erase the block's bounded key space, then write).
	/// These are THE decode entry points; O(1), context-free, strict (no hit ==> raw).
	/// They cover every live filter block of every loaded dataset plus the extras and fallback
	/// blocks, so results decoding needs no per-dataset sweep anymore (mds-plan D1.5 retired the
	/// instance-rooted decode shims, decodeJsonSafeHtml and decodeJsonAcrossEncoders with their
	/// currentEncoder-flip dance).
	static	std::string			decode(const std::string & in)									{ auto it = _decodeTable().find(in);	return it == _decodeTable().end() ? in : it->second.name;				}
	static	bool				shouldDecode(const std::string & in)							{ return _decodeTable().count(in) > 0;																		}
	static	columnType			columnTypeFromEncoded(const std::string & in)					{ auto it = _decodeTable().find(in);	return it == _decodeTable().end() ? columnType::unknown : it->second.type;	}
	///< Replace all occurences of table tokens in `text` by their decoded names.
	static	std::string			decodeAll(const std::string & text);
	///< Replace all occurences of table tokens in all json-names and string-values of `json`.
	static	void				decodeJson(Json::Value & json, bool replaceNames = true, bool safeHtml = false);
	///< Forget every block (test isolation). Live instances re-mint on their next rebuild.
	static	void				resetForTests();

	static	colsPlusTypes		encodeColumnNamesinOptions(Json::Value & options, bool preloadingData);

	/// Multi-dataset aware version of encodeColumnNamesinOptions(): walks options and their .meta in lockstep and
	/// encodes every option against the encoder that `encoderFor` resolves for the (dataSetId, filterId) stamp
	/// recorded in that option's meta (mds-plan D1.6: the stamped filter's VIEW, so an option encodes in exactly
	/// the namespace of the filter its terms were selected through - the same view the slice for that filter is
	/// handed to R in). Options stamped without a filterId (or meta-less ones) carry filterId -1 and fall back to
	/// the resolver's dataset-level choice, which for the legacy path is the process-global current encoder.
	/// Because the resolved encoder is a filter VIEW whose tokens embed both ids, the slices of different datasets
	/// and filters can safely coexist in one options blob. The collected cols+types are returned per dataSetId;
	/// options without a dataSetId in their meta are attributed to `primaryDataSetId`. A null `encoderFor`
	/// reproduces encodeColumnNamesinOptions() exactly.
	static	perDataSetColsPlusTypes	encodeColumnNamesinOptionsPerDataSet(Json::Value & options, bool preloadingData, const EncoderFor & encoderFor, int primaryDataSetId);

	/// Collects the dataSetId -> filterId pairs referenced from an options .meta subtree (see
	/// BoundControlBase::createMeta(), which stamps both onto options that contain variables when the
	/// analysis is multi-dataset aware). A dataSetId seen without filterId is registered as -1 and only
	/// upgraded by a later non -1 occurrence, never downgraded.
	static	void				collectDataSetIdsFromMeta(const Json::Value & meta, std::map<int, int> & dataSetFilterIds);
	///< Like collectDataSetIdsFromMeta, but keeps every distinct (dataSetId, filterId) pair: two
	///< forms selecting two different filters of the same dataset need two slices, and a node
	///< without a filterId contributes (dataSetId, -1) = "the dataset's default".
	static	void				collectFilterIdsFromMeta(const Json::Value & meta, std::set<int> & filterIds);

private:
	static	ColumnEncoder	*	_columnEncoder;
	static	ColumnEncoder	*	_currentEncoder;
	static	ColumnEncoders	*	_otherEncoders;		///< encode-side union members (extras etc.), C5; never a canonical or view

	static	std::map<std::string, DecodeEntry> &	_decodeTable();					///< token -> (original name, type), C4

						ColumnEncoder(CanonicalTag);
						ColumnEncoder(ColumnEncoder * canonical, std::string prefix, std::string postfix);

	void				buildMint(const colTypeMap & namesWithTypes);				///< canonical/FULL: (re)build name<->index state
	void				materializeMaps();											///< FULL only: derive the legacy string maps from mint + own prefix
	void				rebuildBlock();												///< THE block writer: erase bounded key space of the last write, then write
	void				eraseBlock();												///< erase this instance's block from the table
	void				orphanViews();												///< canonical dtor: erase + detach every live view before dying
	bool				canonicalMode() const	{ return _mode == Mode::canonical;	}
	const std::map<std::string, size_t> & mintForward() const						{ return _mode == Mode::view && _canonical ? _canonical->_mintForward : _mintForward;	}
	const std::vector<std::pair<std::string, columnType>> & mintReverse() const		{ return _mode == Mode::view && _canonical ? _canonical->_mintReverse : _mintReverse;	}

	static	void				_convertPreloadingDataOption(Json::Value & option, const std::string& optionName, colsPlusTypes& colTypes, ColumnEncoder * encoder);
	static	void				_addTypeToColumnNamesInOptionsRecursively(Json::Value & options, const Json::Value & meta, int dataSetId, int filterId, bool preloadingData, perDataSetColsPlusTypes & colTypesPerDataSet, const EncoderFor & encoderFor);
	static	void				_encodeColumnNamesinOptions(Json::Value & options, Json::Value & meta, int dataSetId, int filterId, const EncoderFor & encoderFor);
	static	ColumnEncoder	*	_encoderForDataSetId(int dataSetId, int filterId, const EncoderFor & encoderFor);
	static	int					_dataSetIdFromMetaNode(const Json::Value & metaNode, int inherited);
	static	int					_filterIdFromMetaNode(const Json::Value & metaNode, int inherited);

private:
	static	std::string			replaceAll(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names);
	static  std::string			replaceAllStrict(const std::string & text, const std::map<std::string, std::string> & map);

	static	void				replaceAll(Json::Value & json, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, bool replaceNames, bool replaceStrict);
	static	std::vector<size_t>	getPositionsColumnNameMatches(const std::string & text, const std::string & columnName);
			void				collectExtraEncodingsFromMetaJson(const Json::Value & in, colTypeMap & namesCollected) const;
	static	void				sortVectorBigToSmall(std::vector<std::string> & vec);

	//Encode-side map builders rooted at a specific encoder (FULL reads its materialized maps,
	//VIEW composes canonical indices with its own prefix on the fly; both union in the extra
	//encodings from _otherEncoders exactly like before - C5).
			colMap				ownEncodingMap() const;
			colVec				ownOriginalNames() const;
			colVec				ownEncodedNames() const;
	static	colMap				encodingMap(const ColumnEncoder * self);
	static	colVec				originalNames(const ColumnEncoder * self);

	Mode						_mode					= Mode::full;

	// mint (canonical + FULL): name (bare or qualified) -> index, and index -> (original, type)
	std::map<std::string, size_t>					_mintForward;
	std::vector<std::pair<std::string, columnType>>	_mintReverse;

	// legacy materialized string maps (FULL only, derived from mint + own prefix)
	colMap						_encodingMap,
								_decodingMap;
	colVec						_originalNames,
								_encodedNames;
	colTypeMap					_decodingTypes,
								_dataSetTypes;

	ColumnEncoders				_views;					///< canonical: live views whose blocks this mint drives
	ColumnEncoder			*	_canonical	= nullptr;	///< view: non-owning mint

	std::string					_encodePrefix  = "JaspColumn_",
								_encodePostfix = "_Encoded";
	size_t						_blockCount		= 0;	///< indices last written to the table under the block prefix/postfix below
	std::string					_blockPrefix,
								_blockPostfix;
	bool						_writesBlock	= true;	///< false for the replacer-encoder and the canonical (they register nowhere, C5/C2)
};

#endif // COLUMNENCODER_H
