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

#ifndef TESTCOLUMNENCODERCONTEXT_H
#define TESTCOLUMNENCODERCONTEXT_H

#include <QObject>

class TestColumnEncoderContext : public QObject
{
	Q_OBJECT

private slots:
	void init();
	void cleanup();
	void scopedContextsDecodeInterleavedAndRestoreLiveState();
	void malformedContextDoesNotMutateLiveState();
	void replayNeverMutatesLiveEncodersAndNestsExactly();
	void currentEncoderReTargetsOnSwitchAndClearsOnDestruction();
	void encodePerDataSetRoutesOptionsToEncoderOfTheirDataSet();
	void encodePerDataSetRoutesOptionsToStampedFilterView();
	void encodePerDataSetAttributedToPrimaryWithoutProvenance();
	void encodePerDataSetWithoutResolverIsLegacyEquivalent();
	void collectDataSetIdsFromMetaGathersPairsAndUpgrades();
	void collectFilterIdsFromMetaGathersEveryDistinctFilter();
	void decodeJsonCoversEveryBlockAndHonoursSafeHtml();
	// mds-plan Phase 1: canonical mint + per-filter views + the one global decode table
	void canonicalAnswersNoFullStringEncodeDecode();
	void filterViewsMintDistinctTokensThatBothDecode();
	void remintRewritesEveryViewBlockAndStaleTokensGoRaw();
	void typeOnlyChangeKeepsTokensAndMovesBareDefaults();
	void destroyingViewOrCanonicalErasesBlocks();
	void independentCanonicalsFromIdenticalMapsProduceIdenticalBlocks();
	void extrasStyleInstanceFeedsTheGlobalTable();
};

#endif // TESTCOLUMNENCODERCONTEXT_H
