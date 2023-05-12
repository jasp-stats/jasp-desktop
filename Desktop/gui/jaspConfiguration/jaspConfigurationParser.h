#ifndef JASPCONFIGURATION_PARSER_H
#define JASPCONFIGURATION_PARSER_H

#include "jaspconfiguration.h"

class JASPConfigurationParser
{

public:
    virtual bool parse(JASPConfiguration* target, const QString& input);

    //factory function
    static JASPConfigurationParser* getParser(const QString& inputPath = "", const QString& input = "");

protected:
    // singleton stuff
    virtual JASPConfigurationParser* getInstance();
    JASPConfigurationParser(JASPConfigurationParser& other) = delete;
    void operator=(const JASPConfigurationParser&) = delete;
    JASPConfigurationParser();

private:
    JASPConfigurationParser* _instance = nullptr;


};

#endif // JASPCONFIGURATION_PARSER_H
