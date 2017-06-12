#ifndef OPTIONVARIABLEI_H
#define OPTIONVARIABLEI_H

#include <string>
#include <vector>

class OptionVariableI
{

public:
	virtual std::vector<std::string> variables() const = 0;
	virtual void replaceName(std::string oldName, std::string newName) = 0;
	virtual void removeName(std::string name) = 0;

};

#endif // OPTIONVARIABLEI_H
