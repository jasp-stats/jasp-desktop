#include "option.h"

using namespace std;

Option::Option(string name)
{
	_name = name;
}

string Option::name()
{
	return _name;
}



