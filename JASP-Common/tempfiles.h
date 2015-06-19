#ifndef TEMPFILES_H
#define TEMPFILES_H

#include <string>
#include <vector>

void tempfiles_init(long sessionId);
void tempfiles_attach(long sessionId);
void tempfiles_heartbeat();

void tempfiles_create(const std::string &extension, int id, std::string &root, std::string &relativePath);
void tempfiles_createSpecific(const std::string &name, int id, std::string &root, std::string &relativePath);
std::string tempfiles_createSpecific(const std::string &dir, const std::string &filename);

std::vector<std::string> tempfiles_retrieveList(int id = -1);
std::string tempfiles_sessionDirName();

void tempfiles_deleteList(const std::vector<std::string> &files);

void tempfiles_deleteAll();
void tempfiles_deleteOrphans();



#endif // TEMPFILES_H
