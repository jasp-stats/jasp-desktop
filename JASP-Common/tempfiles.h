#ifndef TEMPFILES_H
#define TEMPFILES_H

#include <string>
#include <vector>

void tempfiles_init(long sessionId);
void tempfiles_attach(long sessionId);
void tempfiles_heartbeat();

std::string tempfiles_create(const std::string &extension = "", int id = -1);

std::vector<std::string> tempfiles_retrieveList(int id = -1);
void tempfiles_deleteList(const std::vector<std::string> &files);

void tempfiles_deleteAll();
void tempfiles_deleteOrphans();



#endif // TEMPFILES_H
