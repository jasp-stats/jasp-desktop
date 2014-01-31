#ifndef UTILS_H
#define UTILS_H

#include <QString>
#include <vector>
#include <string>

std::string fq(const QString &from);
QString tq(const std::string &from);
QStringList tql(const std::vector<std::string> &from);

bool remove(std::vector<std::string> &array, std::string &value);


#endif // UTILS_H
