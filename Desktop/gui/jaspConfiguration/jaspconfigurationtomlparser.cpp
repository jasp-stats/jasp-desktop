#include "jaspconfigurationtomlparser.h"
#include "parserlibs/toml.h"
#include "log.h"
#include "version.h"

JASPConfigurationTOMLParser::JASPConfigurationTOMLParser()
{

}

bool JASPConfigurationTOMLParser::parse(JASPConfiguration *target, const QString &input)
{
	toml::table tbl;

	try
	{
		tbl = toml::parse(input.toStdString());
	}
	catch (const toml::parse_error& err)
	{
		Log::log() << "Parsing failed: " << err << std::endl;
	}

	//jasp version
	std::optional<std::string_view> jaspVersion = tbl["JASPVersion"].value<std::string_view>();
	if(jaspVersion)
		parseVersion(target, jaspVersion.value());

	//enabled modules
	toml::array* modulesToLoad = tbl.get_as<toml::array>("EnabledModules");
	if(modulesToLoad)
		modulesToLoad->for_each([&](toml::value<std::string>& elem) { target->setAdditionalModule(QString(elem.get().c_str())); });

	//root constants
	toml::table* rootConstants = tbl.get_as<toml::table>("Constants");
	if(rootConstants)
		parseConstants(target, *rootConstants);

	//modules -> constants, (analyses -> constants & options)
	toml::table* modules = tbl.get_as<toml::table>("Modules");
	if(modules)
	{
		modules->for_each([&](toml::key key, toml::node& node) {
			if(node.is_table())
				parseModule(target, key.data(), *node.as_table());
		});
	}

	//startup commands
	std::optional<std::string_view> cmds = tbl["StartupCommands"].value<std::string_view>();
	if(cmds)
		target->setStartupCommands(QString(cmds.value().data()));

	return true;
}

bool JASPConfigurationTOMLParser::parseVersion(JASPConfiguration* target, std::string_view version)
{
	try
	{
		target->setJASPVersion(Version(std::string(version)));
	}
	catch(std::runtime_error& e)
	{
		Log::log() << "Corrupt version:" << e.what() << std::endl;
		return false;
	}
	return true;
}

bool JASPConfigurationTOMLParser::parseModule(JASPConfiguration *target, const std::string& module, const toml::table &tbl)
{
	//constants
	const toml::table* constants = tbl.get_as<toml::table>("Constants");
	if(constants)
		parseConstants(target, *constants, module);

	//analyses
	const toml::table* analyses = tbl.get_as<toml::table>("Analyses");
	if(analyses)
	{
		analyses->for_each([&](const toml::key key, const toml::node& node) {
			if(node.is_table())
				parseAnalysis(target, module, key.data(), *node.as_table());
		});
	}
	return true;
}

bool JASPConfigurationTOMLParser::parseAnalysis(JASPConfiguration *target, const std::string &module, const std::string &analysis, const toml::table &tbl)
{
	//constants
	const toml::table* constants = tbl.get_as<toml::table>("Constants");
	if(constants)
		parseConstants(target, *constants, module, analysis);

	//options
	const toml::table* options = tbl.get_as<toml::table>("Options");
	if(options)
	{
		options->for_each([&](const toml::key key, const toml::node& node) {
			if(node.is_value())
			{
				QVariant res;
				if(toQVariant(node, res))
					target->addOption(key.data(), res, false, QString(module.data()), QString(analysis.data()));
			}
			else if(node.is_table())
			{
				const toml::table& option = *node.as_table();
				std::optional<bool> lock = option["Lock"].value<bool>();
				toml::node_view value = option["Value"];
				QVariant res;
				if(value && lock && toQVariant(*value.node(), res))
					target->addOption(key.data(), res, lock.value(), QString(module.data()), QString(analysis.data()));
			}
		});
	}
	return true;

}

bool JASPConfigurationTOMLParser::parseConstants(JASPConfiguration *target, const toml::table &tbl, const std::string &module, const std::string& analysis)
{
	tbl.for_each([&](const toml::key key, const toml::node& value) {
		QVariant res;
		if(toQVariant(value, res))
			target->addConstant(QString(key.data()), res, QString(module.c_str()), QString(analysis.c_str()));
	});

	return true;
}


//this can probably be done more elegantly
//might want to expand with dates etc
bool JASPConfigurationTOMLParser::toQVariant(const toml::node &node, QVariant& res)
{
	switch (node.type()) {
	case toml::node_type::string:
		res.setValue(node.value<std::string>().value());
		break;
	case toml::node_type::integer:
		res.setValue(node.value<int64_t>().value());
		break;
	case toml::node_type::floating_point:
		res.setValue(node.value<double>().value());
		break;
	case toml::node_type::boolean:
		res.setValue(node.value<bool>().value());
		break;
	default:
		return false;
	}
	return true;
}

