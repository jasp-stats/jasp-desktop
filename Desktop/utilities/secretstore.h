//
// SecretStore — general-purpose secure credential storage.
//
// Encrypts secrets using libsodium (crypto_secretbox) with a master key
// derived from platform-specific machine + user identifiers.  Ciphertext
// is persisted via the existing QSettings infrastructure.
//
// The master-key derivation lives in a single static method so that it
// can later be swapped for an OS-keychain-backed key without touching
// any of the encrypt/decrypt/store logic.
//
// Usage:
//   QString encrypted = SecretStore::encryptValue("sk-abc");
//   QString decrypted = SecretStore::decryptValue(encrypted);
//

#ifndef SECRETSTORE_H
#define SECRETSTORE_H

#include <QString>
#include "utilities/settings.h"

class SecretStore
{
public:
	/// Read a secret.  Decrypts the ciphertext stored in Settings.
	/// On first run after upgrading from the old qtkeychain-based code,
	/// any plaintext value in Settings is migrated (encrypted) automatically.
	static QString read(const QString &logicalKey, Settings::Type setting);

	/// Write a secret.  Encrypts with the master key and persists to Settings.
	static void write(const QString &logicalKey, const QString &value, Settings::Type setting);

	/// Remove a secret from Settings.
	static void remove(const QString &logicalKey, Settings::Type setting);

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
	/// Defined in secretstore.cpp.
	static const unsigned char kMasterKeySeed[32];

	/// Returns the 32-byte master key (lazily derived, cached for the process lifetime).
	static QByteArray masterKey();

	/// Derive a key from platform + user identity, keyed with kMasterKeySeed.
	static QByteArray deriveMasterKey();

	// --- symmetric encryption (libsodium crypto_secretbox) ----------------

	static QByteArray encrypt(const QByteArray &plaintext, const QByteArray &key);
	static QByteArray decrypt(const QByteArray &blob,     const QByteArray &key);

	// --- Settings I/O -----------------------------------------------------

	static QString    readSetting(Settings::Type setting);
	static void       writeSetting(Settings::Type setting, const QString &value);
	static void       deleteSetting(Settings::Type setting);

	// --- migration --------------------------------------------------------
	static void migrateIfNeeded(Settings::Type setting);
};

#endif // SECRETSTORE_H
