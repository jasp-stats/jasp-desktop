//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "factorsformbase.h"
#include "qutils.h"
#include "variableslistbase.h"
#include "variableinfo.h"
#include "filter.h"
#include "log.h"

using namespace std;

FactorsFormBase::FactorsFormBase(QQuickItem *parent)
	: JASPListControl(parent), BoundControlBase(this)
{
	_controlType			= ControlType::FactorsForm;
	_useControlMouseArea	= false;
	_mayUseFormula			= false;
	_useTermsInRSyntax		= false;
}

void FactorsFormBase::setUpModel()
{
	_factorsModel = new ListModelFactorsForm(this);

	JASPListControl::setUpModel();

	_availableVariablesListName = property("availableVariablesListName").toString();
	QVariant availableListVariant = property("availableVariablesList");
	_availableVariablesListItem = qobject_cast<JASPListControl *>(availableListVariant.value<QObject *>());

	connect(this, &FactorsFormBase::initializedChanged, this, &FactorsFormBase::countVariablesChanged);
	connect(_factorsModel, &ListModelFactorsForm::modelReset, this, &FactorsFormBase::factorsTitlesChanged);
	connect(_factorsModel, &ListModelFactorsForm::modelReset, this, &FactorsFormBase::factorsItemsChanged);
	connect(_factorsModel, &ListModelFactorsForm::rowsInserted, this, &FactorsFormBase::factorsTitlesChanged);
	connect(_factorsModel, &ListModelFactorsForm::rowsInserted, this, &FactorsFormBase::factorsItemsChanged);
	connect(_factorsModel, &ListModelFactorsForm::rowsRemoved, this, &FactorsFormBase::factorsTitlesChanged);
	connect(_factorsModel, &ListModelFactorsForm::rowsRemoved, this, &FactorsFormBase::factorsItemsChanged);
}

void FactorsFormBase::bindTo(const Json::Value& value)
{
	Json::Value newValue = value;
	ListModelFactorsForm::FactorVec factors;

	for (Json::Value& factor : newValue)
	{
		int termId = 0;
		Terms initTerms;
		const Json::Value& termsJson = factor[fq(_optionKeyValue)];
		//mds-plan post: the D5.1 Storage shape per factor is `{value: [term objects], optionKey?}`
		//(no outer `types` - types live inside the term objects); the engine/legacy shapes are
		//`{value, types}` or a bare value. All three unwrap below; only the term-entry parsing
		//differs (the per-term objects keep their provenance, the engine shapes parse unqualified).
		Json::Value valuePart, typesPart;
		if (_isValueWithTypes(termsJson))
		{
			valuePart = termsJson["value"];
			typesPart = termsJson["types"];
		}
		else if (termsJson.isObject() && termsJson.isMember("value"))
			valuePart = termsJson["value"];
		else
			valuePart = termsJson;
		for (Json::Value& termJson : valuePart)
		{
			std::vector<std::string> components;

			//mds-plan post: a D5.1 Storage term object parses INTO its provenance (C9) - plain
			//`{name, types, filterId?}`, or the interaction row shape with the term object nested
			//under the optionKey (the Terms::getStorageOptions discipline, mirrored here because
			//the FactorsForm keeps its own applier instead of the Terms(json) ctor). Everything
			//else keeps the engine/legacy parse byte-identically below.
			const Json::Value & nestedKey = termJson.isObject() && termJson.isMember(fq(_optionKeyValue)) ? termJson[fq(_optionKeyValue)] : Json::Value::null;
			const Json::Value & storageTerm = Term::isStorageTermObject(nestedKey) ? nestedKey : (Term::isStorageTermObject(termJson) ? termJson : Json::Value::null);

			if (!storageTerm.isNull())
			{
				const Term term = Term::fromStorageTermObject(storageTerm);
				initTerms.add(term);
				//normalize the bound value to the engine shape (string, or component array under
				//allowInteraction) so the committed option value stays the frozen wire shape
				if (allowInteraction())
				{
					Json::Value termArray(Json::arrayValue);
					for (const QString &component : term.components())
						termArray.append(fq(component));
					valuePart[termId] = termArray;
				}
				else
					valuePart[termId] = fq(term.value());
				termId++;
				continue;
			}

			if (termJson.isArray())
			{
				// For interaction, each term is an array of strings
				for (const Json::Value& elt : termJson)
					if (elt.isString())
						components.push_back(elt.asString());
			}
			else if (termJson.isString())
				// If not, each term is just a string
				components.push_back(termJson.asString());

			if (components.size() > 0)
			{
				columnTypeVec types;
				if (typesPart.size() > termId) // If the type is given, use it
				{
					if (typesPart[termId].isArray())
						for (const Json::Value& jsonType : typesPart[termId])
							types.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));
					else
						types.push_back(columnTypeFromString(typesPart[termId].asString(), columnType::unknown));
				}
				else
					for (const std::string& component : components)
						types.push_back(model()->getVariableRealType(tq(component)));
				initTerms.add(Term(components, types));
			}
			termId++;
		}

		factors.push_back(ListModelFactorsForm::Factor(tq(factor["name"].asString()), tq(factor["title"].asString()), initTerms));

		Json::Value newTerms = Json::objectValue;
		newTerms["value"] = valuePart;
		newTerms["types"] = initTerms.types();
		factor[fq(_optionKeyValue)] = newTerms;
	}

	BoundControlBase::bindTo(newValue);
	
	_factorsModel->initFactors(factors);
}

Json::Value FactorsFormBase::createJson() const
{
	Json::Value result(Json::arrayValue);

	for (int i = 0; i < _initNumberFactors; i++)
	{
		Json::Value row(Json::objectValue);
		row["name"] = fq(baseName() + QString::number(i + startIndex()));
		row["title"] = fq(baseTitle() + " " + QString::number(i + startIndex()));
		row[fq(_optionKeyValue)] = Json::Value(Json::objectValue);

		result.append(row);
	}
		
	return result;
}

bool FactorsFormBase::isJsonValid(const Json::Value &value) const
{
	bool valid = value.isArray();
	if (valid)
	{
		for (const Json::Value& factor : value)
		{
			valid = factor.isObject() && factor["name"].isString() && factor["title"].isString() && factor.isMember(fq(_optionKeyValue));
			if (!valid) break;
		}
	}

	return valid;
}

void FactorsFormBase::termsChangedHandler()
{
	// This slot is called to set the boundValues if the factors are changed.
	// This does not have to be called during the initialization of the FactorsForm: the _factorsModel might give the wrong information
	// and during the initialization, the boundValues has to be set by the bindTo method anyway.
	if (!initialized()) return;

	JASPListControl::termsChangedHandler();

	setBoundValue(createEngineValue());
}

//mds-plan D3.1: the live factors-model value; written by termsChangedHandler and generated by the
//Engine/Storage generators alike. The per-factor term lists carry their own typed provenance in
//the listView models (mds-plan C9); since the mds-plan post the Engine sidecar is the per-factor
//ARRAY-OF-ARRAYS (createFilterIds above) and the Storage shape carries the per-term objects
//(createStorageValue above) - the D3.2-era option-level-scalar deferral (this comment's original
//subject) is closed.
Json::Value FactorsFormBase::createEngineValue() const
{
	const ListModelFactorsForm::FactorVec &factors = _factorsModel->getFactors();
	Json::Value boundValue(Json::arrayValue);
	
	for (const auto &factor : factors)
	{
		const Terms& terms = factor.listView ? factor.listView->model()->terms() : factor.initTerms;
		Json::Value valuePart(Json::arrayValue),
					typesPart = terms.types();

		for (const Term &term : terms)
		{
			Json::Value termJson(allowInteraction() ? Json::arrayValue : Json::stringValue);
			if (allowInteraction())
			{
				for (const std::string & elt : term.scomponents())
					termJson.append(elt);
			}
			else
				termJson = fq(term.value());
			valuePart.append(termJson);
		}
		Json::Value factorJson(Json::objectValue),
					termsJson(Json::objectValue);
		factorJson["name"] = fq(factor.name);
		factorJson["title"] = fq(factor.title);
		termsJson["value"] = valuePart;
		termsJson["types"] = terms.types();
		factorJson[fq(_optionKeyValue)] = termsJson;
		boundValue.append(factorJson);
	}
	
	return boundValue;
}

//mds-plan post (per-factor provenance): the transient sidecar of this option is an ARRAY-OF-ARRAYS,
//element i = factor i's per-term filterId array - POSITIONAL over that factor's terms exactly like
//BoundControlTerms::createFilterIds is over a plain list's (Term::filterId() when the term is
//qualified, -1 entries backfilled with the option's host id so per-position encode behaviour equals
//the old option-level scalar for every existing form, null only for a hostless unqualified term).
//Factors are POSITIONAL in the bound value (plain array of objects {name, title, <optionKey>}), so
//no named-children grammar and no factor-name stability problem. allowInteraction needs no special
//case: combination terms carry a UNIFORM term-level filterId (mds-plan D2.1/D6.1), which each
//component shares through the same Term. The node is omitted entirely when nothing is stamped, so
//non-aware/hostless forms generate filterId-less Engine json matching today byte-for-byte. The
//engine walk resolves it factor-positionally then term-positionally (Common/columnencoder.cpp,
//the array-of-objects case documented there).
Json::Value FactorsFormBase::createFilterIds() const
{
	if (!encodeValue() || !_factorsModel)
		return Json::nullValue;

	Json::Value perFactor(Json::arrayValue);
	bool		any		= false;

	for (const auto &factor : _factorsModel->getFactors())
	{
		//createEngineValue's own source choice, mirrored exactly: a factor with an instantiated
		//panel reads the panel's model (the live typed terms), a never-rendered one its initTerms.
		const Terms& terms = factor.listView ? factor.listView->model()->terms() : factor.initTerms;

		//mds-plan post (origin-authoritative, found by the zoo): a panel term that arrives without
		//an origin is resolved at use time through THAT PANEL LIST's own C13 chain
		//(JASPControl::resolvedVarInfo: its selection host, else its source/available chain, else
		//the analysis-level endpoint) - NOT through this FactorsForm's: the form is a container
		//whose own chain does not traverse into the per-factor panels, so the form-level fallback
		//resolves a source-fed panel's bare terms at the analysis level (the zoo's
		//factors-over-assigned-source form stamped them with the analysis filter and the engine
		//refused the raw token). Never-rendered factors have no list, so they keep the form-level
		//fallback exactly as before.
		const int hostId = [&]()
		{
			if (factor.listView)
			{
				VariableInfo * varInfo = factor.listView->resolvedVarInfo();
				Filter * stampFilter = varInfo ? dynamic_cast<Filter *>(varInfo->provider()) : nullptr;
				if (stampFilter && stampFilter->data())
					return stampFilter->id();
			}
			return generatedHostFilterId();
		}();

		Json::Value ids(Json::arrayValue);

		for (const Term& term : terms)
		{
			const int id = term.filterId() >= 0 ? term.filterId() : hostId;
			any = any || id >= 0;
			ids.append(id >= 0 ? Json::Value(id) : Json::Value());
		}

		perFactor.append(ids);
	}

	return any ? perFactor : Json::nullValue;
}

//mds-plan post (per-factor provenance, the D5.1 discipline applied per factor): the Storage file
//shape. The engine value above is the frozen R contract ({value, types} per factor, positional -
//no room for per-term provenance), so the file takes the established per-term objects instead:
//each factor's terms go through Terms::getStorageOptions - simple factors as
//{value: [{name, types, filterId?}, ...]}, interaction factors in the row/optionKey discipline
//({value: [{components: {name: [...], types, filterId?}}], optionKey: "components"}). filterId is
//omitted (never null) at -1, exactly like every other D5.1 family. bindTo parses both this and
//the legacy/engine shape (roundtrip pinned by the qml factors-form provenance suite).
Json::Value FactorsFormBase::createStorageValue() const
{
	if (!_factorsModel)
		return createEngineValue();

	const ListModelFactorsForm::FactorVec &factors = _factorsModel->getFactors();
	Json::Value boundValue(Json::arrayValue);

	for (const auto &factor : factors)
	{
		const Terms& terms = factor.listView ? factor.listView->model()->terms() : factor.initTerms;

		Json::Value factorJson(Json::objectValue);
		factorJson["name"]		= fq(factor.name);
		factorJson["title"]		= fq(factor.title);
		factorJson[fq(_optionKeyValue)] = terms.getStorageOptions(Terms::RelatedValuesPerTerm(), fq(_optionKeyValue), "",
																		allowInteraction(), false, false);
		boundValue.append(factorJson);
	}

	return boundValue;
}

void FactorsFormBase::factorAdded(int index, QVariant item)
{
	VariablesListBase* listView = item.value<VariablesListBase *>();
	if (!listView)
	{
		JASPControl* control = item.value<JASPControl *>();
		Log::log() << "JASP Control " << (control ? control->name() : "") << " is not a VariablesListBase in factorAdded" << std::endl;
		return;
	}
	
	_factorsModel->factorAdded(index, listView);
	
	connect(listView->model(), &ListModel::termsChanged, _factorsModel, &ListModelFactorsForm::resetModelTerms, Qt::QueuedConnection);
	connect(listView->model(), &ListModel::termsChanged, this, &FactorsFormBase::countVariablesChanged);
	connect(listView->model(), &ListModel::termsChanged, _factorsModel, &ListModelFactorsForm::ensureNesting);

	listView->setInitialized();
}


QVariantList FactorsFormBase::factorsTitles() const
{
	QVariantList titles;

	if (!_factorsModel)
		return titles;

	for (auto factorModel : _factorsModel->getFactors())
	{
		QMap<QString, QVariant> map;
		map["label"] = factorModel.title;
		map["value"] = factorModel.name;
		titles.append(map);
	}

	return titles;
}

QVariantList FactorsFormBase::factorsItems() const
{
	QVariantList items;

	if (!_factorsModel)
		return items;

	for (auto factorModel : _factorsModel->getFactors())
		items.append(QVariant::fromValue(factorModel.listView));

	return items;
}


