#ifndef UTILS_H
#define UTILS_H

#include <QString>
#include <list>
#include <string>
#include <vector>

std::string fq(const QString &from);
QString tq(const std::string &from);
QStringList tql(const std::vector<std::string> &from);

#endif // UTILS_H
