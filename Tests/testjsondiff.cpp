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
#include "testjsondiff.h"
#include "jsonutilities.h"

#include <json/json.h>

namespace {

Json::Value parse(const std::string & jsonStr)
{
	Json::Value out;
	if (!Json::Reader().parse(jsonStr, out))
		qFatal("Failed to parse test json: %s", jsonStr.c_str());
	return out;
}

bool patchIsEmpty(const Json::Value & patch)
{
	return patch.isObject() && patch.size() == 0;
}

} // namespace

void TestJsonDiff::equalPrimitives()
{
	QVERIFY(JsonUtilities::jsonValuesEqual(parse("\"abc\""), parse("\"abc\"")));
	QVERIFY(JsonUtilities::jsonValuesEqual(parse("true"), parse("true")));
	QVERIFY(JsonUtilities::jsonValuesEqual(parse("null"), parse("null")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse("\"abc\""), parse("\"abd\"")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse("true"), parse("false")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse("null"), parse("\"null\"")));
}

void TestJsonDiff::equalNumericTolerant()
{
	// Values round-tripped through the plot editor can swap between int and
	// double representation, which must not count as a change.
	QVERIFY(JsonUtilities::jsonValuesEqual(Json::Value(1), Json::Value(1.0)));
	QVERIFY(JsonUtilities::jsonValuesEqual(Json::Value(1.0), Json::Value(1)));
	QVERIFY(JsonUtilities::jsonValuesEqual(Json::Value(2.5), Json::Value(2.5)));
	QVERIFY(!JsonUtilities::jsonValuesEqual(Json::Value(1), Json::Value(2)));
}

void TestJsonDiff::equalNestedObjects()
{
	QVERIFY(JsonUtilities::jsonValuesEqual(parse(R"({"a": {"b": 1}})"), parse(R"({"a": {"b": 1.0}})")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse(R"({"a": {"b": 1}})"), parse(R"({"a": {"b": 2}})")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse(R"({"a": 1})"), parse(R"({"a": 1, "b": 2})")));
}

void TestJsonDiff::equalArrays()
{
	QVERIFY(JsonUtilities::jsonValuesEqual(parse(R"([1, 2, 3])"), parse(R"([1.0, 2.0, 3.0])")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse(R"([1, 2, 3])"), parse(R"([1, 2, 4])")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse(R"([1, 2])"), parse(R"([1, 2, 3])")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse(R"(["f", "m"])"), parse(R"(["m", "f"])")));
}

void TestJsonDiff::unequalValues()
{
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse(R"({"a": 1})"), parse(R"([1])")));
	QVERIFY(!JsonUtilities::jsonValuesEqual(parse("3"), parse("\"3\"")));
}

void TestJsonDiff::diffEmpty()
{
	QVERIFY(patchIsEmpty(JsonUtilities::jsonDiff(Json::objectValue, Json::objectValue)));
}

void TestJsonDiff::diffAddedKeys()
{
	Json::Value patch = JsonUtilities::jsonDiff(parse(R"({"a": 1})"), parse(R"({"a": 1, "references": ["x"]})"));
	QVERIFY(patch.isObject());
	QVERIFY(!patch.isMember("a"));
	QVERIFY(patch.isMember("references"));
	QVERIFY(patch["references"] == parse(R"(["x"])"));
}

void TestJsonDiff::diffChangedLeaf()
{
	Json::Value patch = JsonUtilities::jsonDiff(parse(R"({"title": "old"})"), parse(R"({"title": "new"})"));
	QVERIFY(patch["title"].asString() == "new");
}

void TestJsonDiff::diffRemovedKey()
{
	// RFC 7386: a null in the patch means "remove this key" when applied.
	Json::Value patch = JsonUtilities::jsonDiff(parse(R"({"a": 1, "b": 2})"), parse(R"({"a": 1})"));
	QVERIFY(patch.isMember("b"));
	QVERIFY(patch["b"].isNull());
}

void TestJsonDiff::diffNested()
{
	Json::Value patch = JsonUtilities::jsonDiff(
		parse(R"({"xAxis": {"settings": {"title": "same", "labels": ["f","m"]}, "type": "ScaleDiscrete"}})"),
		parse(R"({"xAxis": {"settings": {"title": "same", "labels": ["f","m"]}, "type": "ScaleDiscrete"}})"));

	// Nothing changed, also not at nested levels.
	QVERIFY(patchIsEmpty(patch));

	patch = JsonUtilities::jsonDiff(
		parse(R"({"xAxis": {"settings": {"title": "old", "labels": ["f","m"]}}})"),
		parse(R"({"xAxis": {"settings": {"title": "new", "labels": ["f","m"]}}})"));

	QVERIFY(patch.isMember("xAxis"));
	QVERIFY(patch["xAxis"].isMember("settings"));
	QVERIFY(patch["xAxis"]["settings"].isMember("title"));
	QVERIFY(!patch["xAxis"]["settings"].isMember("labels"));
	QVERIFY(!patch["xAxis"].isMember("type"));
}

void TestJsonDiff::diffArraysWholesale()
{
	// Arrays are compared (and patched) as a whole, no element-level diffing.
	Json::Value patch = JsonUtilities::jsonDiff(parse(R"({"l": [1, 2]})"), parse(R"({"l": [1, 2, 3]})"));
	QVERIFY(patch["l"].isArray());
	QVERIFY(patch["l"].size() == 3);
}

void TestJsonDiff::diffEqualGivesEmptyPatch()
{
	Json::Value a = parse(R"({"a": 1, "b": [1, 2], "c": {"d": "x"}})");
	QVERIFY(patchIsEmpty(JsonUtilities::jsonDiff(a, a)));
}

void TestJsonDiff::diffNumericTolerant()
{
	// int vs double representation must not produce patch noise
	Json::Value patch = JsonUtilities::jsonDiff(parse(R"({"breaks": [1, 2, 3]})"), parse(R"({"breaks": [1.0, 2.0, 3.0]})"));
	QVERIFY(patchIsEmpty(patch));
}

void TestJsonDiff::diffRoundTrip()
{
	// apply(diff(a, b), a) must reconstruct b.
	Json::Value a = parse(R"({"keep": 1, "change": "old", "drop": [1, 2], "nest": {"x": 1, "y": 2}})");
	Json::Value b = parse(R"({"keep": 1, "change": "new", "nest": {"x": 1, "z": 3}, "add": true})");

	Json::Value patched = a;
	JsonUtilities::jsonApplyPatch(patched, JsonUtilities::jsonDiff(a, b));

	QVERIFY(JsonUtilities::jsonValuesEqual(patched, b));
}

void TestJsonDiff::applyPatchSimple()
{
	Json::Value target = parse(R"({"a": 1})");
	JsonUtilities::jsonApplyPatch(target, parse(R"({"a": 2, "b": "x"})"));

	QVERIFY(target["a"].asInt() == 2);
	QVERIFY(target["b"].asString() == "x");
}

void TestJsonDiff::applyPatchRemovesKey()
{
	Json::Value target = parse(R"({"a": 1, "b": 2})");
	JsonUtilities::jsonApplyPatch(target, parse(R"({"b": null})"));

	QVERIFY(target.isMember("a"));
	QVERIFY(!target.isMember("b"));
}

void TestJsonDiff::applyPatchRecurse()
{
	Json::Value target = parse(R"({"x": {"a": 1, "b": 2}})");
	JsonUtilities::jsonApplyPatch(target, parse(R"({"x": {"b": 3}})"));

	QVERIFY(target["x"]["a"].asInt() == 1);
	QVERIFY(target["x"]["b"].asInt() == 3);
}

void TestJsonDiff::applyPatchWholesaleReplace()
{
	// A non-object patch value replaces whatever was there, including objects
	// and arrays (arrays are not merged element-wise).
	Json::Value target = parse(R"({"references": ["old"]})");
	JsonUtilities::jsonApplyPatch(target, parse(R"({"references": ["new1", "new2"]})"));
	QVERIFY(target["references"].size() == 2);
	QVERIFY(target["references"][0].asString() == "new1");

	// An object patch value over a non-object target also replaces.
	Json::Value target2 = parse(R"({"x": 5})");
	JsonUtilities::jsonApplyPatch(target2, parse(R"({"x": {"a": 1}})"));
	QVERIFY(target2["x"].isObject());
	QVERIFY(target2["x"]["a"].asInt() == 1);
}

void TestJsonDiff::applyPatchOntoNullTarget()
{
	Json::Value target = Json::nullValue;
	JsonUtilities::jsonApplyPatch(target, parse(R"({"a": 1})"));
	QVERIFY(target.isObject());
	QVERIFY(target["a"].asInt() == 1);
}

void TestJsonDiff::applyPatchOntoWrongTypeTarget()
{
	// Applying an object patch onto a target of the wrong type (e.g. an array
	// or a string) resets the target to an object first.
	Json::Value target = parse(R"([1, 2])");
	JsonUtilities::jsonApplyPatch(target, parse(R"({"a": 1})"));
	QVERIFY(target.isObject());
	QVERIFY(target["a"].asInt() == 1);
}

void TestJsonDiff::composeRecursesObjects()
{
	Json::Value patch	= parse(R"({"a": {"x": 1}, "c": [1, 2]})");
	Json::Value next	= parse(R"({"a": {"y": 2}, "b": 3, "c": [3]})");

	Json::Value composed = patch;
	JsonUtilities::jsonComposePatches(composed, next);

	QVERIFY(JsonUtilities::jsonValuesEqual(composed, parse(R"({"a": {"x": 1, "y": 2}, "b": 3, "c": [3]})")));

	// Applying the composed patch equals applying both in turn
	Json::Value target		= parse(R"({"a": {"x": 0, "z": 9}, "b": 0, "c": []})");
	Json::Value stepwise	= target;
	JsonUtilities::jsonApplyPatch(stepwise, patch);
	JsonUtilities::jsonApplyPatch(stepwise, next);

	Json::Value atOnce = target;
	JsonUtilities::jsonApplyPatch(atOnce, composed);

	QVERIFY(JsonUtilities::jsonValuesEqual(stepwise, atOnce));
}

void TestJsonDiff::composeKeepsNull()
{
	Json::Value composed = parse(R"({"a": {"x": 1}, "b": 2})");
	JsonUtilities::jsonComposePatches(composed, parse(R"({"a": null})"));

	QVERIFY(composed.isMember("a"));
	QVERIFY(composed["a"].isNull());
	QVERIFY(composed["b"].asInt() == 2);

	// The null still removes the key when the composed patch is applied
	Json::Value target = parse(R"({"a": {"x": 0}, "b": 0})");
	JsonUtilities::jsonApplyPatch(target, composed);

	QVERIFY(!target.isMember("a"));
	QVERIFY(target["b"].asInt() == 2);
}

void TestJsonDiff::composeOntoNonObject()
{
	Json::Value composed = Json::nullValue;
	JsonUtilities::jsonComposePatches(composed, parse(R"({"a": 1})"));

	QVERIFY(JsonUtilities::jsonValuesEqual(composed, parse(R"({"a": 1})")));
}

void TestJsonDiff::extendEmptyPatchIsDiff()
{
	Json::Value base	= parse(R"({"title": "x", "limits": [0, 1]})");
	Json::Value edited	= parse(R"({"title": "y", "limits": [0, 1], "references": [0]})");

	Json::Value extended = JsonUtilities::jsonExtendPatch(Json::objectValue, base, edited);

	QVERIFY(JsonUtilities::jsonValuesEqual(extended, JsonUtilities::jsonDiff(base, edited)));
	QVERIFY(JsonUtilities::jsonValuesEqual(JsonUtilities::jsonExtendPatch(Json::nullValue, base, edited), extended));
}

void TestJsonDiff::extendKeepsEarlierChanges()
{
	// The base already shows the earlier change, the edit only changes "b"
	Json::Value patch	= parse(R"({"a": "edited"})");
	Json::Value base	= parse(R"({"a": "edited", "b": "plain"})");
	Json::Value edited	= parse(R"({"a": "edited", "b": "edited too"})");

	Json::Value extended = JsonUtilities::jsonExtendPatch(patch, base, edited);

	QVERIFY(extended["a"].asString() == "edited");
	QVERIFY(extended["b"].asString() == "edited too");

	// An edit that changes nothing keeps the patch as it was
	QVERIFY(JsonUtilities::jsonValuesEqual(JsonUtilities::jsonExtendPatch(patch, base, base), patch));
}

void TestJsonDiff::missingKeysKeepsAdditions()
{
	// References exist only in what the desktop sends, never in engine output.
	Json::Value missing = JsonUtilities::jsonKeysMissingFrom(
		parse(R"({"references": [{"value": 1.5}], "xAxis": {"settings": {"title": "x"}}})"),
		parse(R"({"xAxis": {"settings": {"title": "x", "labels": ["f","m"]}}})"));

	QVERIFY(missing.isMember("references"));
	QVERIFY(!missing.isMember("xAxis"));
}

void TestJsonDiff::missingKeysDropsPresentKeys()
{
	// Presence-based: keys the reference also has are dropped even when their
	// values differ, because there the value may be stale engine output.
	Json::Value missing = JsonUtilities::jsonKeysMissingFrom(
		parse(R"({"xAxis": {"settings": {"labels": ["f","m"]}}})"),
		parse(R"({"xAxis": {"settings": {"labels": ["m","f"]}}})"));

	QVERIFY(patchIsEmpty(missing));
}

void TestJsonDiff::missingKeysNested()
{
	Json::Value missing = JsonUtilities::jsonKeysMissingFrom(
		parse(R"({"xAxis": {"settings": {"title": "t", "custom": 1}, "extra": true}})"),
		parse(R"({"xAxis": {"settings": {"title": "t", "labels": []}}})"));

	QVERIFY(missing.isMember("xAxis"));
	QVERIFY(missing["xAxis"].isMember("extra"));
	QVERIFY(missing["xAxis"].isMember("settings"));
	QVERIFY(missing["xAxis"]["settings"].isMember("custom"));
	QVERIFY(!missing["xAxis"]["settings"].isMember("title"));
}

void TestJsonDiff::missingKeysNonObjectSource()
{
	QVERIFY(patchIsEmpty(JsonUtilities::jsonKeysMissingFrom(Json::nullValue, Json::objectValue)));
	QVERIFY(patchIsEmpty(JsonUtilities::jsonKeysMissingFrom(parse(R"([1,2])"), Json::objectValue)));
}

void TestJsonDiff::boxplotResizeLabelBugScenario()
{
	// INTERNAL-jasp issue 3366: descriptives boxplot of contNormal split by
	// facGender. After a resize the desktop used to persist the engine's full
	// editOptions, pinning xAxis.settings.labels. Reordering the factor labels
	// then produced boxes in the new order with ticks in the old one.
	//
	// With the diff-based storage the resize persists nothing, a user edit only
	// stores its actual changes, and reordering the labels afterwards keeps the
	// fresh labels because they never enter the patch.

	// 1. Fresh engine options of run 1: labels in factor order f, m.
	Json::Value baseRun1 = parse(R"({
		"xAxis": { "type": "ScaleDiscretePosition", "settings": { "labels": ["f", "m"], "breaks": [1.0, 2.0] } },
		"yAxis": { "type": "ScaleContinuousPosition", "settings": { "limits": [-3.0, 3.0] } }
	})");

	// 2. User adds a reference line; the editor sends the full options, which
	//    match the base everywhere except the addition.
	Json::Value submittedRun1 = parse(R"({
		"xAxis": { "type": "ScaleDiscretePosition", "settings": { "labels": ["f", "m"], "breaks": [1, 2] } },
		"yAxis": { "type": "ScaleContinuousPosition", "settings": { "limits": [-3, 3] } },
		"references": [ { "value": 0, "type": "population" } ]
	})");

	Json::Value patch = JsonUtilities::jsonDiff(baseRun1, submittedRun1);

	// The patch contains exactly the user's addition and nothing else: no
	// labels, no breaks, no limits (only numeric-tolerant round-trip noise).
	QVERIFY(patch.isMember("references"));
	QVERIFY(!patch.isMember("xAxis"));
	QVERIFY(!patch.isMember("yAxis"));
	QVERIFY(patch.getMemberNames().size() == 1);

	// 3. Run 2 after reordering facGender: fresh labels m, f.
	Json::Value baseRun2 = parse(R"({
		"xAxis": { "type": "ScaleDiscretePosition", "settings": { "labels": ["m", "f"], "breaks": [1, 2] } },
		"yAxis": { "type": "ScaleContinuousPosition", "settings": { "limits": [-3, 3] } }
	})");

	// 4. Materialize the options to re-send: fresh labels survive, the
	//    reference is re-applied.
	Json::Value materialized = baseRun2;
	JsonUtilities::jsonApplyPatch(materialized, patch);

	QVERIFY(materialized["xAxis"]["settings"]["labels"][0].asString() == "m");
	QVERIFY(materialized["xAxis"]["settings"]["labels"][1].asString() == "f");
	QVERIFY(materialized.isMember("references"));
	QVERIFY(materialized["references"][0]["value"].asDouble() == 0.0);

	// 5. The editor shows the materialized options; another edit (axis title)
	//    must extend the patch without pinning the labels.
	Json::Value submittedRun2 = materialized;
	submittedRun2["xAxis"]["settings"]["title"] = "Custom title";

	Json::Value patch2 = JsonUtilities::jsonDiff(baseRun2, submittedRun2);
	QVERIFY(patch2.isMember("references"));
	QVERIFY(patch2.isMember("xAxis"));
	QVERIFY(patch2["xAxis"]["settings"].isMember("title"));
	QVERIFY(!patch2["xAxis"]["settings"].isMember("labels"));
}

void TestJsonDiff::editsSurviveReusedPlotScenario()
{
	// After an edit the engine keeps the edited plot in its state. A run that
	// does not touch the plot's dependencies reuses it, so the editOptions of
	// that run, which become the new diff base, already contain the edit.
	// Recomputing the patch against such a base dropped the earlier edit, and
	// the next run that drew the plot anew lost it for good.

	// 1. Plain plot of the first run
	Json::Value plainBase = parse(R"({
		"xAxis": { "type": "ScaleContinuousPosition", "settings": { "title": "contNormal", "labels": ["-2", "0", "2"] } },
		"yAxis": { "type": "ScaleContinuousPosition", "settings": { "title": "Density" } }
	})");

	// 2. The user sets the x-axis title
	Json::Value submitted1 = plainBase;
	submitted1["xAxis"]["settings"]["title"] = "A";

	Json::Value patch = JsonUtilities::jsonExtendPatch(Json::objectValue, plainBase, submitted1);
	QVERIFY(patch["xAxis"]["settings"]["title"].asString() == "A");

	// 3. An unrelated option changes: the run reuses the edited plot, so its
	//    editOptions, the new base, already show the x-axis title
	Json::Value reusedBase = plainBase;
	reusedBase["xAxis"]["settings"]["title"] = "A";

	// 4. The editor shows base + patch, the user sets the y-axis title
	Json::Value submitted2 = reusedBase;
	JsonUtilities::jsonApplyPatch(submitted2, patch);
	submitted2["yAxis"]["settings"]["title"] = "B";

	// A diff against the reused base alone no longer knows about the x-axis title
	QVERIFY(!JsonUtilities::jsonDiff(reusedBase, submitted2).isMember("xAxis"));

	patch = JsonUtilities::jsonExtendPatch(patch, reusedBase, submitted2);
	QVERIFY(patch["xAxis"]["settings"]["title"].asString() == "A");
	QVERIFY(patch["yAxis"]["settings"]["title"].asString() == "B");
	QVERIFY(!patch["xAxis"]["settings"].isMember("labels"));

	// 5. A run that draws the plot anew: both edits are re-applied
	Json::Value materialized = plainBase;
	JsonUtilities::jsonApplyPatch(materialized, patch);

	QVERIFY(materialized["xAxis"]["settings"]["title"].asString() == "A");
	QVERIFY(materialized["yAxis"]["settings"]["title"].asString() == "B");
}

QTEST_MAIN(TestJsonDiff)
#include "testjsondiff.moc"
