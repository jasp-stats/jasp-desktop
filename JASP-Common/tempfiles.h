#ifndef TEMPFILES_H
#define TEMPFILES_H

#include <string>

void tempfiles_init(long sessionId);
void tempfiles_attach(long sessionId);
void tempfiles_heartbeat();

std::string tempfiles_create(const std::string &extension = "", int id = -1);

void tempfiles_deleteAll();
void tempfiles_deleteOrphans();



#endif // TEMPFILES_H
