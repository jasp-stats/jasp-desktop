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

/// Class to "encode" the names of columns. It runs in three modes:
///
///  * FULL      - self-contained instance: owns a mint (name -> index and index -> (original,
///                type)) AND materializes the full-string encode/decode maps in its own prefix
///                namespace. Its namespace is registered as a block in the process-global strict
///                decode table (see the static decode()). Used by the process-global fallback encoder,
///                the extras encoder (DataBridge's `JaspExtraOptions_`) and ad-hoc callers
///                (tests, ColumnEncoderContext replay).
///
///  * CANONICAL - the per-DataSet mint, created via createCanonical(): keeps only
///                `name -> index` and `index -> (originalName, type)` built by
///                setCurrentNames()/updateColumnTypesOnly() with the classic alphabetical-counter
///                semantics (`.scale`/`.ordinal`/`.nominal` triples + bare-name default). It
///                NEVER holds or answers full-string encode/decode; instead every live VIEW
///                minted on top of it gets its block rewritten whenever the mint changes.
///
///  * VIEW      - the per-Filter namespace handle, created via createView(): a non-owning
///                {canonical*, prefix} pair. encode() = canonical index lookup + own prefix
///                compose, shouldEncode() = canonical membership, no materialized per-view
///                string maps. Its block lives in the global decode table and is erased when
///                the view dies (deleted filter's tokens go raw).
///
/// Decoding is always context-free: the ONE process-global table (a block per live filter
/// of every loaded dataset plus the extras and fallback blocks) is the only decode source,
/// membership-only, unknown tokens stay raw. Encode-side unioning of the registered "other"
/// encoders (extras) survives; all decode-side merging is gone.
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
	/// the resolver should return the dataset's shown-filter view / legacy fallback).
	/// Resolves the encoder a node's names must encode against (consumed per the
	/// sidecar flip). `filterId` is read from the transient `.filterIds` sidecar (scalar node =
	/// the option-wide id, positional entry = per-term id, -1 = unqualified: the legacy
	/// fallback the resolver documents for its context). The resolver may RE-ATTRIBUTE
	/// `dataSetId` (in-out) to the dataset of the namespace it resolved — filter ids imply their
	/// dataset (Filter::data) — which is how the collected cols+types stay keyed per dataset
	/// without the retired meta stamps. A null resolver reproduces the classic behaviour exactly:
	/// process-global current encoder, no re-attribution.
	typedef std::function<ColumnEncoder *(int & dataSetId, int filterId)>	EncoderFor;

	/// One entry of the process-global strict decode table: token -> (original name, type).
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

	/// Create the canonical mint for a DataSet: mint-only, answers no full-string
	/// encode/decode, registers no block itself (its views' blocks are its footprint).
	static ColumnEncoder	*	createCanonical();
	/// Create a view on `canonical` minting `prefix + <index> + postfix` tokens; registers
	/// with the canonical and mints its block in the global decode table right away. Destroying
	/// the view (or the canonical) erases the block again.
	static ColumnEncoder	*	createView(ColumnEncoder * canonical, const std::string & prefix, const std::string & postfix = "_Encoded");

	static ColumnEncoder	*	columnEncoder();
	/// Like columnEncoder() but never consults/repopulates the process-global current-encoder
	/// indirection (_currentEncoder). Used by the desktop so it never depends on the global,
	/// which is only meaningful in the engine's request context.
	static ColumnEncoder	*	fallbackEncoder();
	static ColumnEncoder	*	currentEncoder();
	/// Points the legacy non-aware request-context indirection at `encoder` (this is a VIEW -
	/// the shown dataset's shown filter's handle - never a canonical).
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

	///Find decoded columnNames close to `name`: identical up to case, one being a prefix or substring
	///of the other, or a small edit distance. Returns at most `maxSuggestions`, best match first.
	///Meant for "did you mean"-feedback when something refers to a column that does not exist.
	static	colVec				suggestColumnNames(const std::string & name, size_t maxSuggestions = 3);

			bool				shouldEncode(const std::string & in);

			/// Mint a fresh namespace. On the canonical and on FULL instances this (re)builds the
			/// mint and rewrites every dependent block; on a VIEW it funnels into its canonical
			/// (callers that re-point the current encoder at a view and rename through it - the
			/// legacy shape - mutate the dataset mint with the very blast radius the dataset's own
			/// encoder always had).
			void				setCurrentNames(const colTypeMap & names);
			const colTypeMap&	currentNames() const											{ return _mode == Mode::view && _canonical ? _canonical->_dataSetTypes : _dataSetTypes;	}
			///< The block prefix of this instance ("" on a canonical, which has no namespace).
			std::string			encodePrefix() const											{ return _encodePrefix;																				}
			void				updateColumnTypesOnly(const colTypeMap & names);
			void				setCurrentNames(const std::vector<std::string> & names, bool generateTypesEncoding=true);	///< Do not use! Deprecated
			void				setCurrentColumnTypePerName(const colTypeMap & theMap);									///< Do not use! Deprecated
			void				setCurrentNamesFromOptionsMeta(const Json::Value & json);

			/// raw name (bare or `name.type`) -> full encoded token. Canonical never answers;
			/// a view composes canonical index + own prefix.
			std::string			encode(const std::string &in);

			///Replace all occurences of columnNames in a string by their encoded versions, taking into account the presence of word boundaries and parentheses.
			std::string			encodeRScript(std::string text, std::set<std::string> * columnNamesFound = nullptr);
			std::string			encodeRScript(std::string text, std::map<std::string, std::set<std::string>>& prefixedColumnsFound, const std::set<std::string>& allowedPrefixes);
			std::string			encodeRScript(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, std::set<std::string> * columnNamesFound = nullptr, const std::string& acceptedPrefix = "");

		///Resolve a set of column references as collected by encodeRScript to the columns and types
		///they ask for: an exact columnname gets its current type and a "columnName.type" reference
		///that type. Anything else (a levelName that happens to match, for instance) is skipped:
		///there is nothing to load for it and R can complain about it being missing at lookup time.
			colsPlusTypes		colsPlusTypesFor(const std::set<std::string> & columnNames) const;

		///Replace all occurences of columnNames in a string by their encoded versions, regardless of word boundaries or parentheses.
		///Instance method: encodes against *this* encoder (own namespace + the registered extras)
		/// Decode is context-free and goes through the global table instead.
			std::string			encodeAll(const std::string & text) const;

			///Replace all occurences of columnNames in a string by their encoded versions in all json-names and string-values, regardless of word boundaries or parentheses.
			void				encodeJson(Json::Value & json, bool replaceNames = false, bool replaceStrict = false) const;

	/// --- The ONE process-global strict decode table -------------------------------
	/// Single writer: rebuildBlock() (erase the block's bounded key space, then write).
	/// These are THE decode entry points; O(1), context-free, strict (no hit ==> raw).
	/// They cover every live filter block of every loaded dataset plus the extras and fallback
	/// blocks, so results decoding needs no per-dataset sweep anymore (the instance-rooted decode
	/// shims and their currentEncoder-flip dance retired at).
	static	std::string			decode(const std::string & in)									{ auto it = _decodeTable().find(in);	return it == _decodeTable().end() ? in : it->second.name;				}
	static	bool				shouldDecode(const std::string & in)							{ return _decodeTable().count(in) > 0;																		}
	static	columnType			columnTypeFromEncoded(const std::string & in)					{ auto it = _decodeTable().find(in);	return it == _decodeTable().end() ? columnType::unknown : it->second.type;	}
	///< Replace all occurences of table tokens in `text` by their decoded names.
	static	std::string			decodeAll(const std::string & text);
	///< Replace all occurences of table tokens in all json-names and string-values of `json`.
	static	void				decodeJson(Json::Value & json, bool replaceNames = true, bool safeHtml = false);
	///< Forget every block (test isolation). Live instances re-mint on their next rebuild.
	static	void				resetForTests();

	/// The deterministic mint over a raw column map: index -> (originalName, type), exactly
	/// what buildMint() stores and every block is written from. Pure - no instance, no table.
	static	std::vector<std::pair<std::string, columnType>>	mintReverseOf(const colTypeMap & namesWithTypes);
	/// raw name (bare or `name.type`) -> `prefix + <mint index> + postfix` token under the
	/// deterministic mint of `names` ("" if `in` is not part of that mint). The pure-function
	/// form of encode(): used by the scoped replay blocks and by the token-shape contract tests,
	/// never a substitute for an encoder instance on live paths.
	static	std::string			tokenForNameInNames(const colTypeMap & names, const std::string & in, const std::string & prefix, const std::string & postfix = "_Encoded");

	/// The pre-multi-dataset encode prefix every released JASP used (the class default from
	/// before the D<ds>_F<filter> namespaces): released .jasp files can still hold
	/// `JaspColumn_<i>_Encoded` tokens inside PERSISTED results. See decodeJsonLegacyTokens.
	static constexpr const char * LEGACY_DEFAULT_PREFIX = "JaspColumn_";

	/// best-effort legacy-prefix result decoder for PRE-EXISTING persisted results.
	/// Materializes the released namespace - `LEGACY_DEFAULT_PREFIX + <i> + "_Encoded"` over the
	/// CURRENT dataset column map, same pure mint (mintReverseOf) released JASP minted with -
	/// and sweeps it through every string value and member name of `json`, replacing hits by the
	/// bare original name (exactly what the released instance decoder did).
	///
	/// READ-SIDE ONLY by construction (the single-writer rule): NOTHING is registered into
	/// the global decode table, live decoding stays strict-table, and the sweep is applied once
	/// at file load to stored result trees only (Analyses::createFromJaspFileEntry). The honest
	/// limitation, loud and clear: the namespace can only be rebuilt from the columns as they
	/// are NOW - correct exactly when the dataset has not changed since the file was saved.
	/// Renamed/reordered/deleted columns leave their old tokens RAW or map to whatever the
	/// deterministic mint points at now; deleted filters have no bearing (the legacy format had
	/// none). No grammar parsing: pure mint-index lookup, garbage in, raw out, never a crash.
	static	void				decodeJsonLegacyTokens(Json::Value & json, const colTypeMap & namesWithTypes);

	/// Scoped replay block of the global decode table: writes `prefix +
	/// <mint index of names> + _Encoded -> (name, type)` on construction and, on destruction,
	/// restores EXACTLY the table contents it found under those keys (undo log, not a blind
	/// erase - entries a live block had at the colliding indices come back, never dropped).
	/// The sanctioned scoped exception to the rebuildBlock-single-writer rule: purely
	/// derived, LIFO-nesting, and it is what lets ColumnEncoderContext replay an archived
	/// namespace through the one table without mutating any live canonical/view/extras.
	class ScopedDecodeBlock
	{
	public:
							ScopedDecodeBlock(const std::string & prefix, const colTypeMap & names);
							~ScopedDecodeBlock();
							ScopedDecodeBlock(const ScopedDecodeBlock &) = delete;
		ScopedDecodeBlock &	operator=(const ScopedDecodeBlock &) = delete;

	private:
		std::string											_prefix;
		size_t												_count		= 0;
		std::vector<std::pair<std::string, DecodeEntry>>	_displaced;	///< table entries that occupied our keys on entry
	};

	static	colsPlusTypes		encodeColumnNamesinOptions(Json::Value & options, bool preloadingData);

	/// Multi-dataset aware version of encodeColumnNamesinOptions(): walks options, their .meta and
	/// the transient `.filterIds` sidecar (schema, grammar at sidecarOwnFilterId) in
	/// one pass and encodes every option against the encoder that `encoderFor` resolves for the
	/// filterId that governs the node (the stamped filter's VIEW, so an option
	/// encodes in exactly the namespace of the filter its terms were selected through - the same
	/// view the slice for that filter is handed to R in). A POSITIONAL sidecar array encodes the
	/// option's terms one by one through their own filters (mixed provenance within one list);
	/// nodes the sidecar says nothing about carry filterId -1 and fall back to the resolver's
	/// dataset-level choice, which for the legacy path is the process-global current encoder.
	/// Because the resolved encoder is a filter VIEW whose tokens embed both ids, the slices of
	/// different datasets and filters can safely coexist in one options blob. The collected
	/// cols+types are returned per dataSetId: attributed through the resolver's re-attachment
	/// (see EncoderFor), everything the sidecar does not name stays with `primaryDataSetId`.
	/// A null `encoderFor` reproduces encodeColumnNamesinOptions() exactly - sidecar or not
	/// (classic encoding always runs against the current encoder).
	static	perDataSetColsPlusTypes	encodeColumnNamesinOptionsPerDataSet(Json::Value & options, bool preloadingData, const EncoderFor & encoderFor, int primaryDataSetId);

	// ----------------------------------------------------------------------------------------------
	// `.filterIds` sidecar (generators produce, request encoding consumes, stripped at the
	// engine boundary before R sees the options). Grammar of one sidecar node, placed at the
	// option's path inside the root tree `options[FILTERIDS_MEMBER]`:
	//   int               option-level filterId; covers the whole option subtree (the inherit
	//                     analogue of what an enclosing .meta stamp did in the old lockstep).
	//   array(int|null)   POSITIONAL over the option's term values (aligned like `types`): each
	//                     entry is the term's filterId, null = unqualified term (resolves through
	//                     the caller's legacy fallback). A positional node never inherits
	//                     parent->child, which structurally avoids the old provenance-meta walk's
	//                     parent-stamp leak into child nodes.
	// array(array(int|null)) PER-FACTOR positional (FactorsForm family): the
	//                     option value is an array of factor OBJECTS (each carrying its terms
	//                     under the optionKey) and element i is factor i's per-term array, resolved
	//                     factor-positionally then term-positionally. The discriminator is the
	//                     option VALUE's element shape (objects ⟹ per-factor) AND the sidecar
	//                     element shape (arrays) - the two co-occur only for the per-factor
	//                     generator, so no existing flat family can misfire (documented at the
	//                     walk sites in columnencoder.cpp).
	//   object            container: named members are child-option nodes (row controls placed
	//                     under this option's path); the reserved member FILTERIDS_SELF ("$")
	//                     carries this option's own int|array node when one exists and would
	//                     otherwise collide with the named children (the generator demotes to a
	//                     container on collision, see AnalysisForm::placeOptionFilterIds).
	//   null / absent     nothing said here: the enclosing resolution is inherited.
	static constexpr const char * FILTERIDS_MEMBER	= ".filterIds";
	static constexpr const char * FILTERIDS_SELF	= "$";

	///< The int filterId a sidecar node asserts for its own option (bare int, or the container's
	///< FILTERIDS_SELF int), -1 when the node says nothing scalar (positional array, container
	///< without "$", null).
	static	int					sidecarOwnFilterId(const Json::Value & sidecarNode);
	///< The positional int|null array a sidecar node asserts over the option's term values (bare
	///< array, or the container's FILTERIDS_SELF array), nullptr when there is none.
	static	const Json::Value *	sidecarPositional(const Json::Value & sidecarNode);
	///< The child node a container sidecar node carries for the option `optionName` (null when the
	///< node is not a container or names no child; FILTERIDS_SELF is never a child lookup).
	static	Json::Value			sidecarChild(const Json::Value & sidecarNode, const std::string & optionName);
	///< Collects every int leaf of a `.filterIds` sidecar tree (scalars, positional arrays and
	///  "$"-containers alike) - the queue input of DataBridge::prepareMultiDataSetRun and
	///  of the component-side reference computation. The provenance-meta collectors of the
	/// stamped-mirror era are DELETED with the mirror itself: the
	///  sidecar is now the one place filter ids live.
	static	void				collectFilterIdsFromSidecar(const Json::Value & sidecarNode, std::set<int> & filterIds);
	// ----------------------------------------------------------------------------------------------

private:
	static	ColumnEncoder	*	_columnEncoder;
	static	ColumnEncoder	*	_currentEncoder;
	static	ColumnEncoders	*	_otherEncoders;		///< encode-side union members (extras etc.); never a canonical or view

	static	std::map<std::string, DecodeEntry> &	_decodeTable();					///< token -> (original name, type)

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

	static	void				_convertPreloadingDataOption(Json::Value & option, const std::string& optionName, perDataSetColsPlusTypes & colTypesPerDataSet, int dataSetId, int filterId, const Json::Value & sidecar, const EncoderFor & encoderFor);
	static	void				_handleTermOptionObject(Json::Value & parentObject, const std::string & optionName, int dataSetId, int filterId, const Json::Value & sidecar, bool preloadingData, perDataSetColsPlusTypes & colTypesPerDataSet, const EncoderFor & encoderFor);
	static	void				_addTypeToColumnNamesInOptionsRecursively(Json::Value & options, const Json::Value & meta, const Json::Value & sidecar, int dataSetId, int filterId, bool preloadingData, perDataSetColsPlusTypes & colTypesPerDataSet, const EncoderFor & encoderFor);
	static	void				_encodeColumnNamesinOptions(Json::Value & options, Json::Value & meta, const Json::Value & sidecar, int dataSetId, int filterId, const EncoderFor & encoderFor);
	static	ColumnEncoder	*	_encoderForDataSetId(int & dataSetId, int filterId, const EncoderFor & encoderFor);

private:
	static	std::string			replaceAll(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names);
	static  std::string			replaceAllStrict(const std::string & text, const std::map<std::string, std::string> & map);

	static	void				replaceAll(Json::Value & json, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, bool replaceNames, bool replaceStrict);
	static	std::vector<size_t>	getPositionsColumnNameMatches(const std::string & text, const std::string & columnName);
	static	void				collectColumnNamesFromStringLiterals(const std::string & text, const colMap & map, std::set<std::string> & found);
			void				collectExtraEncodingsFromMetaJson(const Json::Value & in, colTypeMap & namesCollected) const;
	static	void				sortVectorBigToSmall(std::vector<std::string> & vec);

	//Encode-side map builders rooted at a specific encoder (FULL reads its materialized maps,
	//VIEW composes canonical indices with its own prefix on the fly; both union in the extra
	//encodings from _otherEncoders exactly like before).
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
	bool						_writesBlock	= true;	///< false for the replacer-encoder and the canonical (they register nowhere)
};

#endif // COLUMNENCODER_H
