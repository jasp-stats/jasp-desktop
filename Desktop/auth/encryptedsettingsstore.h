//
// EncryptedSettingsStore — obfuscated secret storage inside QSettings.
//
// INTERNAL TO SecretVault (auth/secretvault.h): nothing outside the vault
// includes this file. It is the vault's fallback for Degrade::ToEncryptedSettings
// secrets, and the one-time migration reader for values written before the
// vault existed.
//
// Encrypts secrets with libsodium (crypto_secretbox) under a master key derived
// from machine + user identity and a seed compiled into the binary. Be honest
// about what that is: the seed ships in an open-source binary, so this is
// obfuscation, not confidentiality. It raises the bar against casual file
// reading, nothing more.
//
// Usage:
//   QString encrypted = EncryptedSettingsStore::encryptValue("sk-abc");
//   QString decrypted = EncryptedSettingsStore::decryptValue(encrypted);
//

#ifndef ENCRYPTEDSETTINGSSTORE_H
#define ENCRYPTEDSETTINGSSTORE_H

#include <QString>

class EncryptedSettingsStore
{
public:
	/// Encrypt a value and return it as a base64 string suitable for embedding
	/// in any JSON or text blob (no QSettings involvement).
	/// Returns the plaintext as-is if encryption is unavailable.
	static QString encryptValue(const QString &plaintext);

	/// Decrypt a value previously produced by encryptValue().
	/// Returns the plaintext as-is if decryption fails or encryption is unavailable.
	static QString decryptValue(const QString &ciphertextBase64);

private:
	// --- master key -------------------------------------------------------
	//
	// Currently derived from platform-specific machine + user material,
	// keyed with a compiled-in seed so the key cannot be reproduced from
	// machine identity alone.
	//
	// Replace deriveMasterKey() with an OS-vault fetch when you want to
	// store the master key in macOS Keychain / Windows Credential Store /
	// Linux libsecret.  The rest of the class stays identical.

	/// Fixed seed for the keyed hash — random bytes, unique to JASP.
	/// Sized for BLAKE2b key material (max 64 bytes).
	/// Defined in encryptedsettingsstore.cpp.
	static const unsigned char kMasterKeySeed[32];

	/// Returns the 32-byte master key (lazily derived, cached for the process lifetime).
	static QByteArray masterKey();

	/// Derive a key from platform + user identity, keyed with kMasterKeySeed.
	static QByteArray deriveMasterKey();

	// --- symmetric encryption (libsodium crypto_secretbox) ----------------

	static QByteArray encrypt(const QByteArray &plaintext, const QByteArray &key);
	static QByteArray decrypt(const QByteArray &blob,     const QByteArray &key);
};

#endif // ENCRYPTEDSETTINGSSTORE_H
