#include "jaspencrypt.h"

#include <sodium.h>
#include <stdexcept>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <filesystem>
#include "json/json.h"
#include "base64.h"


//    ┌──────────────────────────┐                                                                      File Info JSON:
//  ▲ │                          │
//  │ │                          │
// 8│ │        FileMagic:        │             format_version                              uint
//  │ │    0x9a42ffa3cf2777c0    │             data_length                                 uint64                      y - 48
//  ▼ │                          │             crypt_ciphertext_length                     uint64                      y
//    ├──────────────────────────┤             crypt_encryption_nonce                      base64 encoded string       Encryption Nonce (crypto_box_curve25519xsalsa20poly1305_NONCEBYTES 24B)
//  ▲ │                          │             crypt_password_hash_salt                    base64 encoded string       Password Hash Salt (Argon2id 16 bytes)
//  │ │                          │             crypt_public_key                            base64 encoded string       crypto_box_curve25519xsalsa20poly1305_PUBLICKEYBYTES 32B (only present if asym)
// 8│ │    Format Version #: 1   │             crypt_asymetric_encryption                  bool
//  │ │                          │             crypt_public_key_sender                     base64 encoded string       crypto_box_curve25519xsalsa20poly1305_PUBLICKEYBYTES 32B (only present if asym)
//  ▼ │                          │             additional items
//    ├──────────────────────────┤
//  ▲ │                          │                        Cipher Text:
//  │ │                          │
// 8│ │    File Info Size <x>    │                 ┌──────────────────────────┐
//  │ │                          │               ▲ │                          │
//  ▼ │                          │               │ │                          │
//    ├──────────────────────────┤             16| │     Encryption MAC       │
//  ▲ │                          │               │ │                          │
//  │ │                          │               ▼ │                          │
//  │ │                          │                 ├──────────────────────────┤
// x│ │        File Info         │               ▲ │                          │
//  │ │                          │               │ │        Encrypted         │
//  │ │                          │             32│ │      File Info Hash      │
//  ▼ │                          │               │ │                          │
//    ├──────────────────────────┤               ▼ │                          │
//  ▲ │                          │                 ├──────────────────────────┤
//  │ │                          │               ▲ │                          │
//  │ │                          │               │ │        Encrypted         │
// y│ │       Cipher Text        │           y-48│ │      JASP File Data      │
//  │ │                          │               │ │                          │
//  │ │                          │               ▼ │                          │
//  ▼ │                          │                 ├──────────────────────────┤
//    └──────────────────────────┘

const uint64_t magic = 0x9a42ffa3cf2777c0;
const uint64_t fileFormatVersion = 1;
const uint64_t infoFormatVersion = 1;
const uint64_t headerHashSize = 32;

int deriveKeyPairFromPassword(std::string_view password, unsigned char* passwordHashSalt, unsigned char* publickey, unsigned char* secretkey) {
    unsigned char keygenSeed[crypto_box_SEEDBYTES];
    if (crypto_pwhash
        (keygenSeed, sizeof keygenSeed, password.data(), password.size(), passwordHashSalt,
         crypto_pwhash_OPSLIMIT_MODERATE, crypto_pwhash_MEMLIMIT_MODERATE,
         crypto_pwhash_ALG_DEFAULT) != 0) {
        throw std::runtime_error("We ran out of memory in while generating a key");
    }
    crypto_box_seed_keypair(publickey, secretkey, keygenSeed);
    return 0;
}

int deriveKeyPairFromPrivateKey(std::string_view b64PrivKey, unsigned char* publickey, unsigned char* secretkey) {
    std::string tmp = base64::from_base64(b64PrivKey);
    if(tmp.size() != crypto_box_SECRETKEYBYTES) throw std::runtime_error("File corrupt: base64 decode of secretkey resulted in string of inappropriate length");
    std::memcpy(secretkey, tmp.data(), crypto_box_SECRETKEYBYTES);
    crypto_scalarmult_base(publickey, secretkey);
    return 0;
}

namespace JASPEncrypt {

int encrypt(const std::filesystem::path& unencryptedJASPFile, const std::filesystem::path& encryptedJASPFile, const std::string_view secret, Json::Value& fileInfo, const std::string_view optionalPublickeyReceiver, const std::string_view forcedPasswordSalt, bool secretIsPrivKey) {

	//hash password, derive keypair and generate the necessary nonce values for this encryption stunt
	unsigned char publickey[crypto_box_PUBLICKEYBYTES];
	unsigned char secretkey[crypto_box_SECRETKEYBYTES];
	unsigned char encryptionNonce[crypto_box_NONCEBYTES];
	unsigned char passwordHashSalt[crypto_pwhash_SALTBYTES];

    randombytes_buf(passwordHashSalt, sizeof passwordHashSalt);
    randombytes_buf(encryptionNonce, sizeof encryptionNonce);

    if(secretIsPrivKey)
        deriveKeyPairFromPrivateKey(secret, publickey, secretkey);
    else
        deriveKeyPairFromPassword(secret, passwordHashSalt, publickey, secretkey);

	//allocate space for header hash & crypto MAC and read file data
	std::vector<unsigned char> data(crypto_box_MACBYTES + headerHashSize);
	if(std::ifstream source_file { unencryptedJASPFile, std::ios::binary }; source_file)
		data.insert(data.end(), std::istreambuf_iterator<char>{source_file}, {});
	else
		throw std::runtime_error("Could not open: " + unencryptedJASPFile.generic_string());


	//add crypto and format details to file info and get a valid json string
	bool asymetric = optionalPublickeyReceiver.length() > 0;
    fileInfo["format_version"] = infoFormatVersion;
	fileInfo["data_length"] = data.size() - crypto_box_MACBYTES - headerHashSize;
	fileInfo["crypt_encryption_nonce"] = base64::to_base64(std::string_view(reinterpret_cast<char*>(encryptionNonce), sizeof encryptionNonce));
    fileInfo["crypt_password_hash_salt"] = forcedPasswordSalt.length() ? std::string(forcedPasswordSalt) : base64::to_base64(std::string_view(reinterpret_cast<char*>(passwordHashSalt), sizeof passwordHashSalt));
    fileInfo["crypt_public_key"] = asymetric ? std::string(optionalPublickeyReceiver) : "";
    fileInfo["crypt_public_key_sender"] = asymetric ? base64::to_base64(std::string_view(reinterpret_cast<char*>(publickey), sizeof publickey)) : "";
	fileInfo["crypt_ciphertext_length"] = (uint64_t)data.size();
	fileInfo["crypt_asymetric_encryption"] = asymetric; //for submissions to jasp, company internal, or something special

	//copy the provided publickey of the receiver into our the publickey array for encryption
	if(asymetric) {
		std::string tmp = base64::from_base64(optionalPublickeyReceiver);
		if(tmp.size() > sizeof publickey) throw std::runtime_error("The provided public key is of insufficient length");
		std::memcpy(publickey, tmp.data(), sizeof publickey);
	}

	Json::StreamWriterBuilder writer;
	std::string fileInfoString = Json::writeString(writer, fileInfo);
	size_t fileInfoSize = fileInfoString.size();

	//gather the header bytes in one sopt
	std::vector<unsigned char> header;
	auto appendhelper = [&](auto value) {
		auto view_value = std::string_view(reinterpret_cast<const char*>(&value), sizeof value);
		header.insert(header.end(), view_value.begin(), view_value.end());
	};
	appendhelper(magic);
	appendhelper(fileFormatVersion);
	appendhelper(fileInfoSize);
	header.insert(header.end(), fileInfoString.begin(), fileInfoString.end());

	//hash the header and insert it in the data buff
	unsigned char headerHash[headerHashSize];
	crypto_generichash(headerHash, sizeof headerHash, header.data(), header.size(), NULL, 0);
	auto hash_view =  std::string_view(reinterpret_cast<const char*>(&headerHash), headerHashSize);
	std::copy(hash_view.begin(), hash_view.end(), data.begin() + crypto_box_MACBYTES); //insert hash of the entire header as a check against god


	//encrypt
	if (crypto_box_easy(data.data(), data.data() + crypto_box_MACBYTES, data.size() - crypto_box_MACBYTES, encryptionNonce, publickey, secretkey) != 0) {
		throw std::runtime_error("Encryption failed");
	}

	//write to output
	if(std::ofstream sink_file { encryptedJASPFile, std::ios::binary }; sink_file) {
		sink_file.write(reinterpret_cast<const char*>(header.data()), header.size());
		sink_file.write(reinterpret_cast<const char*>(data.data()), data.size());
		sink_file.close();
	}
	else
		throw std::runtime_error("Could not open output file: " + encryptedJASPFile.generic_string());

	return 0;
}


int decrypt(const std::filesystem::path& unencryptedJASPFile, const std::filesystem::path& encryptedJASPFile, const std::string_view secret, Json::Value& fileInfo, std::string& responsePublickey, std::string& responsePasswordSalt, bool secretIsPrivKey) {

	//read data and parse static fields from header
    responsePublickey = "";
    responsePasswordSalt = "";
	std::vector<unsigned char> data;
	if(std::ifstream source_file { encryptedJASPFile, std::ios::binary }; source_file)
		data.insert(data.end(), std::istreambuf_iterator<char>{source_file}, {});
	else
		throw std::runtime_error("Could not open: " + encryptedJASPFile.generic_string());

	uint64_t readMagic, fileFormatVersion, fileInfoSize;
	const uint64_t static_header_size = sizeof magic + sizeof fileFormatVersion + sizeof fileInfoSize;
	if(data.size() > static_header_size) {
		readMagic = *(reinterpret_cast<uint64_t*>(data.data()));
		fileFormatVersion = *(reinterpret_cast<uint64_t*>(data.data() + sizeof magic));
		fileInfoSize = *(reinterpret_cast<uint64_t*>(data.data() + sizeof magic + sizeof fileFormatVersion));
	}
	else
		throw std::runtime_error("Encrypted file: " + encryptedJASPFile.generic_string() + " is corrupt. Not enough bytes");


	//compare magic just to be sure
	if(readMagic != magic)
		throw std::runtime_error("FileMagic# does not match");


	//read the fileInfo JSON
	if(data.size() > static_header_size + fileInfoSize) {
		auto fileInfoStr = std::string_view(reinterpret_cast<char*>(data.data() + static_header_size), fileInfoSize);

		Json::CharReaderBuilder builder;
		std::unique_ptr<Json::CharReader> reader(builder.newCharReader());
		std::string errs;

		bool success = reader->parse(fileInfoStr.data(), fileInfoStr.data() + fileInfoStr.size(), &fileInfo, &errs);
		if (!success)
			throw std::runtime_error("Unable to parse file info json: " + errs);
	}
	else
		throw std::runtime_error("Encrypted file: " + encryptedJASPFile.generic_string() + " is corrupt. Not enough bytes");

	//hash the header for later comparison with the one in the ciphertext
	std::vector<unsigned char> headerHash(headerHashSize);
	crypto_generichash(headerHash.data(), headerHash.size(), data.data(), static_header_size + fileInfoSize, NULL, 0);


	//extract the crypto and size details from the fileInfo JSON
	auto extractHelper = [&](unsigned char * buf, uint64_t size,  const std::string name) {
		std::string tmp = base64::from_base64(fileInfo[name].asString());
		if(tmp.size() > size) throw std::runtime_error("File corrupt: base64 decode of " + name + " resulted in string of inappropriate length");
		std::memcpy(buf, tmp.data(), size);
	};
	unsigned char read_publickey[crypto_box_PUBLICKEYBYTES]; extractHelper(read_publickey, sizeof read_publickey, "crypt_public_key");
	unsigned char read_publickey_sender[crypto_box_PUBLICKEYBYTES]; extractHelper(read_publickey_sender, sizeof read_publickey_sender, "crypt_public_key_sender");
	unsigned char encryptionNonce[crypto_box_NONCEBYTES]; extractHelper(encryptionNonce, sizeof encryptionNonce, "crypt_encryption_nonce");
	unsigned char passwordHashSalt[crypto_pwhash_SALTBYTES]; extractHelper(passwordHashSalt, sizeof passwordHashSalt, "crypt_password_hash_salt");
	uint64_t infoFormatVersion = fileInfo["format_version"].asUInt64();
	uint64_t dataLength = fileInfo["data_length"].asUInt64();
	uint64_t ciphertextLength = fileInfo["crypt_ciphertext_length"].asUInt64();
	bool asymetric = fileInfo["crypt_asymetric_encryption"].asBool();


	unsigned char secretkey[crypto_box_SECRETKEYBYTES];
	unsigned char publickey[crypto_box_PUBLICKEYBYTES];
    if(secretIsPrivKey)
        deriveKeyPairFromPrivateKey(secret, publickey, secretkey);
    else
        deriveKeyPairFromPassword(secret, passwordHashSalt, publickey, secretkey);

    if(asymetric) { //check which public key should be used for decrypt.
        bool receiver = std::string_view(reinterpret_cast<char*>(&publickey), sizeof publickey) == std::string_view(reinterpret_cast<char*>(&read_publickey), sizeof read_publickey);
        bool creator = std::string_view(reinterpret_cast<char*>(&publickey), sizeof publickey) == std::string_view(reinterpret_cast<char*>(&read_publickey_sender), sizeof read_publickey_sender);
        if(receiver) {
            responsePublickey = base64::to_base64(std::string_view(reinterpret_cast<char*>(&read_publickey_sender), sizeof read_publickey_sender));
            std::memcpy(publickey, read_publickey_sender, sizeof publickey);
        }
        else if(creator) {
            responsePublickey = base64::to_base64(std::string_view(reinterpret_cast<char*>(&read_publickey), sizeof read_publickey));
            std::memcpy(publickey, read_publickey, sizeof publickey);
        }
        else
            throw std::runtime_error("Password does not match either sender of receiver");
        responsePasswordSalt = base64::to_base64(std::string_view(reinterpret_cast<char*>(&passwordHashSalt), sizeof passwordHashSalt));
    }

	if(data.size() < + static_header_size + fileInfoSize + ciphertextLength)
		throw("Encrypted file: " + encryptedJASPFile.generic_string() + " is corrupt. Not enough bytes");

	// Decrypt
	if (crypto_box_open_easy(data.data(), data.data() + static_header_size + fileInfoSize, ciphertextLength, encryptionNonce, publickey, secretkey))
		throw std::runtime_error("File corrupt: decrypt failed");
	data.resize(ciphertextLength - crypto_box_MACBYTES);

	//read the header hash that was stored in the ciphertext
	std::vector<unsigned char> readHeaderHash(headerHashSize);
	std::copy(data.begin(), data.begin() + headerHashSize, readHeaderHash.begin());
	if(!(readHeaderHash == headerHash))
		throw std::runtime_error("File corrupted. Someone/something altered the file header.");


	//write out decrypted data
	if(std::ofstream sink_file { unencryptedJASPFile, std::ios::binary }; sink_file) {
		sink_file.write(reinterpret_cast<const char*>(data.data() + headerHashSize), dataLength);
		sink_file.close();
	}
	else
		throw std::runtime_error("Could not open output file: " + encryptedJASPFile.generic_string());

	return 0;
}


bool detectEncryptedJASPFile(const std::filesystem::path &file)
{
	uint64_t detectedMagic = 0;
	if(std::ifstream source_file { file, std::ios::binary }; source_file) {
		source_file.read(reinterpret_cast<char*>(&detectedMagic), sizeof detectedMagic);
		source_file.close();
	}
	else
		return false;
	return	detectedMagic == magic;
}

// int main() {
// 	writeEncryptedFile("in.jasp", "out.jasp", "password", {});
// 	Json::Value x;
// 	readEncryptedFile("decrypt.jasp", "out.jasp", "password", x);

// 	return 0;
// }

}
