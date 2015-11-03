#ifndef TEMPFILES_H
#define TEMPFILES_H

#include <string>
#include <vector>

/*
 * Each running UI process creates a series of temporary directories
 * Within these temporary directories, subdirectories exist, numbered
 * with the analysisId, for each analysis.
 * In these the state file exists, as well as all the plots.
 *
 * In the event that the UI process crashes, and fails to clean up
 * after itself, these 'orphans' are deleted next time the program
 * starts up. This performed by the deleteOrphans() function below.
 *
 * During normal operation, the UI process 'touch'es a status file
 * in the temp directory, every 30 seconds or so this is handled by
 * the heartbeat() function below.
 *
 * deleteOrphans() checks the modification time of this status file
 * and if it is more than a minute old, the temp directory is
 * considered orphaned and is deleted.
 */

void tempfiles_init(long sessionId);
void tempfiles_attach(long sessionId);
void tempfiles_heartbeat();

std::string tempfiles_createSpecific_clipboard(const std::string &filename);
void tempfiles_purgeClipboard();

void tempfiles_create(const std::string &extension, int id, std::string &root, std::string &relativePath);
void tempfiles_createSpecific(const std::string &name, int id, std::string &root, std::string &relativePath);
std::string tempfiles_createSpecific(const std::string &dir, const std::string &filename);

std::vector<std::string> tempfiles_retrieveList(int id = -1);
std::string tempfiles_sessionDirName();

void tempfiles_deleteList(const std::vector<std::string> &files);

void tempfiles_deleteAll();
void tempfiles_deleteOrphans();



#endif // TEMPFILES_H
