#ifndef JASPCONFIGURATION_PARSER_H
#define JASPCONFIGURATION_PARSER_H

#include "jaspconfiguration.h"
#include "peglib/peglib.h"


class JASPConfigurationParser
{

public:
	bool parse(JASPConfiguration* target, const QString& input);

	//singleton stuff
	static JASPConfigurationParser* getInstance();
	JASPConfigurationParser(JASPConfigurationParser& other) = delete;
	void operator=(const JASPConfigurationParser&) = delete;

	struct KeyValue { QString key; QVariant value; };
	struct Option { KeyValue keyValue; bool locked; };
	struct Analysis { QString name; std::vector<Option> options; std::vector<KeyValue> constants; };
	struct Module { QString name; std::vector<Analysis> analyses; std::vector<KeyValue> constants; };
	using LoadModulesList = QStringList;

private:
	JASPConfigurationParser();

	static JASPConfigurationParser* _instance;

	static std::any parseBool(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseInt(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseFloat(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseString(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseName(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseValue(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseKeyValuePair(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseVersion(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseLoadModuleList(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseOptionDef(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseOptionStmt(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseAnalysisStmt(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseModuleStmt(const peg::SemanticValues &vs, std::any& dt);
	static std::any parseJASPConf(const peg::SemanticValues &vs, std::any& dt);





	peg::parser _parser;
	bool _validGrammar;
	const char* _jaspConfigGrammar = R"(
		JASPConf			<- Format? Statement* Version Statement*

		Statement			<- ModuleStmt / LoadModuleList / KeyValuePair / Comment
		ModuleStmt			<- 'Module' Name Comment? ( AnalysisStmt / KeyValuePair / Comment )* 'End Module' Comment?
		AnalysisStmt		<- 'Analysis' Name Comment? ( OptionStmt / KeyValuePair / Comment )* 'End Analysis' Comment?
		OptionStmt			<- 'Options' Comment? OptionDef* 'End Options' Comment?
		OptionDef			<- KeyValuePair Comment? / KeyValuePair 'lock'? Comment?

		LoadModuleList    	<- 'Enable Modules' ':' List(Name, ',') Comment?
		Format				<- 'Format' ':' < [0-9.]* > Comment?
		Version				<- 'JASP_Version' ':' < [0-9.]* >  Comment?
		KeyValuePair		<- Name '=' Value Comment?

		Value				<-	Bool / Float / Int / StringLiteral / Name
		Name        		<- < [a-zA-Z_][a-zA-Z_0-9]* >
		StringLiteral		<-  '"' < [^"]* > '"'
		Digits				<- [0-9]+
		Int     			<- < [-+]?  Digits >
		Float				<- < Int '.' Digits > / < Int 'e' Int >
		Bool				<- ('true' / 'True' / 'TRUE') / ('false' / 'False' / 'FALSE')

		~Comment			<- '#' (!EndOfLine .)* EndOfLine
		EndOfLine			<- '\r\n' / '\n' / '\r'
		%whitespace 		<- [ \t\n]*

		# Macros
		List(I, D) ← I (D I)*
	)";

};

#endif // JASPCONFIGURATION_PARSER_H
