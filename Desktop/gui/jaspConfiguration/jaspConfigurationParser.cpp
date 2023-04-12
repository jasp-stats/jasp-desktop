#include "jaspConfigurationParser.h"
#include "log.h"

#include<iostream>

JASPConfigurationParser* JASPConfigurationParser::_instance = nullptr;

JASPConfigurationParser* JASPConfigurationParser::getInstance()
{
	if(!_instance)
		_instance = new JASPConfigurationParser();
	return _instance;
}

JASPConfigurationParser::JASPConfigurationParser()
{
	_parser.set_logger([](size_t line, size_t col, const std::string& msg, const std::string &rule) {
		Log::log() << "JASPConfiguration parse error: line" << line << " column:" << col << ": " << msg << "\n";
	});

	_validGrammar = _parser.load_grammar(_jaspConfigGrammar);
	if(!_validGrammar)
		Log::log() << "Specified Grammar not valid. Recommend peglint to investigate" << std::endl;

	_parser["JASPConf"] = parseJASPConf;
	_parser["ModuleStmt"] = parseModuleStmt;
	_parser["AnalysisStmt"] = parseAnalysisStmt;
	_parser["OptionStmt"] = parseOptionStmt;
	_parser["OptionDef"] = parseOptionDef;
	_parser["LoadModuleList"] = parseLoadModuleList;
	_parser["Version"] = parseVersion;
	_parser["KeyValuePair"] = parseKeyValuePair;
	_parser["Value"] = parseValue;
	_parser["Name"] = parseName;
	_parser["StringLiteral"] = parseString;
	_parser["Int"] = parseInt;
	_parser["Float"] = parseFloat;
	_parser["Bool"] = parseBool;
}

std::any JASPConfigurationParser::parseBool(const peg::SemanticValues &vs, std::any &dt)
{
	switch (vs.choice()) {
	case 0: //true
		return true;
	default: //false
		return false;
	}
}

bool JASPConfigurationParser::parse(JASPConfiguration* target, const QString &input)
{
	try {
		std::any dt = target;
		return _parser.parse(input.toStdString(), dt);
	}
	catch (std::exception& e) {
		Log::log() << e.what() << std::endl;
	}
	return false;
}

std::any JASPConfigurationParser::parseInt(const peg::SemanticValues &vs, std::any &dt)
{
	return vs.token_to_number<long long>();
}

std::any JASPConfigurationParser::parseFloat(const peg::SemanticValues &vs, std::any &dt)
{
	return vs.token_to_number<double>();
}

std::any JASPConfigurationParser::parseString(const peg::SemanticValues &vs, std::any &dt)
{
	return QString(vs.token_to_string().c_str());
}

std::any JASPConfigurationParser::parseName(const peg::SemanticValues &vs, std::any &dt)
{
	return QString(vs.token_to_string().c_str());
}

std::any JASPConfigurationParser::parseValue(const peg::SemanticValues &vs, std::any &dt)
{
	switch (vs.choice()) {
	case 0: //Bool
		return QVariant(any_cast<bool>(vs[0]));
	case 1: //Float
		return QVariant(any_cast<double>(vs[0]));
	case 2: //Int
		return QVariant(any_cast<long long>(vs[0]));
	case 3: //StringLiteral
		return QVariant(any_cast<QString>(vs[0]));
	case 4: //Name
		return QVariant(any_cast<QString>(vs[0]));
	default:
		return QVariant(any_cast<QString>(vs[0]));
	}
}

std::any JASPConfigurationParser::parseKeyValuePair(const peg::SemanticValues &vs, std::any& dt)
{
	return KeyValue{any_cast<QString>(vs[0]), any_cast<QVariant>(vs[1])};
}

std::any JASPConfigurationParser::parseVersion(const peg::SemanticValues &vs, std::any &dt)
{
	try
	{
		Version v(vs.token_to_string());
		return v;
	}
	catch (std::runtime_error& e)
	{
		throw std::runtime_error("Could not parse JASP Version number in configuration file: " + std::string(e.what()));
	}
}

std::any JASPConfigurationParser::parseLoadModuleList(const peg::SemanticValues &vs, std::any &dt)
{
	QStringList list;
	for(int i = 0; i < vs.size(); i++)
		list.push_back(any_cast<QString>(vs[i]));
	return list;
}

std::any JASPConfigurationParser::parseOptionDef(const peg::SemanticValues &vs, std::any &dt)
{
	switch (vs.choice()) {
	case 0: //no lock
		return Option{any_cast<KeyValue>(vs[0]), false};
	default: //lock
		return Option{any_cast<KeyValue>(vs[0]), true};
	}
}

std::any JASPConfigurationParser::parseOptionStmt(const peg::SemanticValues &vs, std::any &dt)
{
	std::vector<Option> options;
	for(auto& option : vs)
		options.push_back(any_cast<Option>(option));
	return options;
}

std::any JASPConfigurationParser::parseAnalysisStmt(const peg::SemanticValues &vs, std::any &dt)
{
	Analysis res;
	res.name = any_cast<QString>(vs[0]);
	for(int i = 1; i < vs.size(); i++)
	{
		if(vs[i].type() == typeid(std::vector<Option>)) //OptionStmt
		{
			std::vector<Option> options = any_cast<std::vector<Option>>(vs[i]);
			res.options.insert(res.options.end(), options.begin(), options.end());
		}
		else if(vs[i].type() == typeid(KeyValue)) //KeyValuePair
			res.constants.push_back(any_cast<KeyValue>(vs[i]));
	}
	return res;
}

std::any JASPConfigurationParser::parseModuleStmt(const peg::SemanticValues &vs, std::any &dt)
{
	Module res;
	res.name = any_cast<QString>(vs[0]);
	for(int i = 1; i < vs.size(); i++)
	{
		if(vs[i].type() == typeid(Analysis)) //AnalysisStmt
			res.analyses.push_back(any_cast<Analysis>(vs[i]));
		else if(vs[i].type() == typeid(KeyValue)) //KeyValuePair
			res.constants.push_back(any_cast<KeyValue>(vs[i]));
	}
	return res;
}

std::any JASPConfigurationParser::parseJASPConf(const peg::SemanticValues &vs, std::any &dt)
{
	JASPConfiguration* conf = any_cast<JASPConfiguration*>(dt);
	for(int i = 0; i < vs.size(); i++)
	{
		if(vs[i].type() == typeid(Module)) //ModuleStmt
		{
			QString moduleName = any_cast<Module>(vs[i]).name; //module constants
			for(KeyValue& constant : any_cast<Module>(vs[i]).constants)
				conf->addConstant(constant.key, constant.value, moduleName);

			for(Analysis& analysis : any_cast<Module>(vs[i]).analyses) //analyses
			{
				QString analysisName = analysis.name;
				for(KeyValue& constant : analysis.constants) //analyses constants
					conf->addConstant(constant.key, constant.value, moduleName, analysisName);

				for(Option& option : analysis.options) //analyses constants
					conf->addOption(option.keyValue.key, option.keyValue.value, option.locked, moduleName, analysisName);
			}
		}
		else if(vs[i].type() == typeid(KeyValue)) //global constants
		{
			conf->addConstant(any_cast<KeyValue>(vs[i]).key, any_cast<KeyValue>(vs[i]).value);
		}
		else if(vs[i].type() == typeid(LoadModulesList)) //LoadModulesList
		{
			conf->_modulesToLoad += any_cast<LoadModulesList>(vs[i]);
		}
		else if(vs[i].type() == typeid(Version)) //Version
		{
			conf->_jaspVersion = any_cast<Version>(vs[i]);
		}
	}
	return conf;
}
