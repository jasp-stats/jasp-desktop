#ifndef JASPCONFIGURATIONTOMLPARSER_H
#define JASPCONFIGURATIONTOMLPARSER_H

#include "jaspconfigurationparser.h"
#include "parserlibs/toml.h"

class JASPConfigurationTOMLParser : public JASPConfigurationParser
{
public:
    JASPConfigurationTOMLParser();

	bool parse(JASPConfiguration* target, const QString& input) override;

private:
	bool parseVersion(JASPConfiguration* target, std::string_view version);
	bool parseModule(JASPConfiguration* target, const std::string& module,  const toml::table& tbl);
	bool parseAnalysis(JASPConfiguration* target, const std::string& module, const std::string& analysis,  const toml::table& tbl);
	bool parseConstants(JASPConfiguration* target, const toml::table& tbl, const std::string &module = "", const std::string& analysis = "");
	bool toQVariant(const toml::node& node, QVariant& res);

};

#endif // JASPCONFIGURATIONTOMLPARSER_H
