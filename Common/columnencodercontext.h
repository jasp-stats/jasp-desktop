//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef COLUMNENCODERCONTEXT_H
#define COLUMNENCODERCONTEXT_H

#include "columnencoder.h"

#include <optional>

/// A captured column-encoder namespace, replayable through the process-global decode table.
///
/// v2 (mds-plan D1.7): replay is a pair of scoped table blocks (ColumnEncoder::ScopedDecodeBlock)
/// - registered on scope entry, undone on exit - and it NEVER mutates a live encoder: not the
/// canonical mint, not a filter view, not the extras encoder. The context is the captured column
/// set plus the namespace prefix its tokens live under (the host filter's `JASPColumn_D<ds>_F<f>_`
/// prefix since Phase 1; "" = replay under whatever prefix is live, the v1 contract).
/// Wire shape is unchanged from v1 (version/columns/extra/optional prefix); v1 contexts still
/// parse and replay with the same semantics, new captures are stamped version 2.
class ColumnEncoderContext
{
public:
	static constexpr int Version = 2;

	ColumnEncoderContext() = default;
	ColumnEncoderContext(const ColumnEncoder::colTypeMap & columns, const ColumnEncoder::colTypeMap & extra,
	                     const std::string & prefix = std::string());

	static ColumnEncoderContext	fromJson(const Json::Value & context);
	static ColumnEncoderContext	fromJsonString(const char * contextJson);

	Json::Value					toJson() const;

	const ColumnEncoder::colTypeMap&	columns() const		{ return _columns; }
	const ColumnEncoder::colTypeMap&	extra() const		{ return _extra; }
	///< The encode namespace (prefix) the captured columns were minted in ("" = replay under
	///< whatever prefix is live). Filter views mint per-filter prefixed names
	///< (JASPColumn_D<dataSetId>_F<filterId>_), so a context captured on one dataset must carry
	///< its prefix to replay correctly once another dataset is live.
	const std::string&					prefix() const		{ return _prefix; }
	bool								supplied() const	{ return _supplied; }

private:
	ColumnEncoder::colTypeMap	_columns;
	ColumnEncoder::colTypeMap	_extra;
	std::string					_prefix;
	bool						_supplied = false;
};

/// Scoped replay of a captured context (v2): installs the captured columns as a scoped block of
/// the global decode table (under the context's prefix, or the live namespace's prefix when the
/// context carries none) and the captured extra encodings as a scoped block under the extras
/// encoder's prefix. Nested scopes restore exactly (LIFO undo log); a malformed context throws
/// from the parse before anything is written, so the live table can never be corrupted by one.
/// The extras encoder (and every live dataset) is never consulted for anything but its prefix -
/// its state is not mutated, and live namespaces keep decoding while a replay is in scope
/// (decode is a superset over live blocks, C4; colliding indices resolve to the replayed context).
class ScopedColumnEncoderContext
{
public:
	ScopedColumnEncoderContext(const ColumnEncoderContext & context, ColumnEncoder & extraEncoder);
	~ScopedColumnEncoderContext();

	ScopedColumnEncoderContext(const ScopedColumnEncoderContext &) = delete;
	ScopedColumnEncoderContext & operator=(const ScopedColumnEncoderContext &) = delete;

private:
	bool											_supplied		= false;
	std::optional<ColumnEncoder::ScopedDecodeBlock>	_columnsBlock;
	std::optional<ColumnEncoder::ScopedDecodeBlock>	_extraBlock;
};

Json::Value decodeColumnJson(const char * payloadJson, const char * encoderContextJson, ColumnEncoder & extraEncoder, bool replaceNames = true);

#endif // COLUMNENCODERCONTEXT_H
