#ifndef JASPENCRYPT_H
#define JASPENCRYPT_H

#include <cstdint>
#include <filesystem>
#include "json/json.h"

namespace JASPEncrypt {

bool detectEncryptedJASPFile(const std::filesystem::path& file);
int encrypt(const std::filesystem::path& unencryptedJASPFile, const std::filesystem::path& encryptedJASPFile, const std::string_view secret, Json::Value& fileInfo, const std::string_view optionalPublickeyReceiver = "");
int decrypt(const std::filesystem::path& unencryptedJASPFile, const std::filesystem::path& encryptedJASPFile, const std::string_view secret, Json::Value& fileInfo, bool secretIsPrivKey);

}

#endif // JASPENCRYPT_H
