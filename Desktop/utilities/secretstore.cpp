//
// SecretStore implementation.
//
// Encryption:       libsodium crypto_secretbox (XSalsa20-Poly1305)
// Key derivation:   crypto_generichash (BLAKE2b) from machine + user identity
// Storage backend:  QSettings (existing Settings infrastructure)
//

#include "secretstore.h"

#include <sodium.h>

#include <QDir>
#include <QFile>
#include <QStandardPaths>
#include <QMutex>
#include <QMutexLocker>

#if defined(Q_OS_MACOS)
#include <sys/sysctl.h>
#endif

#include "log.h"

// =============================================================================
// Master-key seed — defined here so it stays out of every translation unit
// that includes secretstore.h.
// =============================================================================

const unsigned char SecretStore::kMasterKeySeed[32] = {
	0x7e, 0x3a, 0x91, 0x4c, 0xd2, 0x6f, 0x88, 0x15,
	0x3b, 0xae, 0x72, 0x59, 0xc1, 0x4d, 0x0f, 0xe8,
	0x66, 0x2d, 0x97, 0x53, 0xa4, 0x1b, 0x38, 0xcc,
	0xfd, 0x09, 0x85, 0x7b, 0x12, 0x4e, 0x6a, 0xd0
};

// =============================================================================
// Master key — derived once, cached forever
// =============================================================================

QByteArray SecretStore::masterKey()
{
	static QByteArray key;
	static QMutex mutex;
	QMutexLocker lock(&mutex);

	if (key.isEmpty()) {
		if (sodium_init() < 0) {
			Log::log() << "SecretStore: libsodium init failed — secrets will be stored as plaintext"
					   << std::endl;
			return QByteArray(); // empty key → store in plaintext
		}
		key = deriveMasterKey();
		Log::log() << "SecretStore: master key derived (" << key.size() << " bytes)" << std::endl;
	}
	return key;
}

QByteArray SecretStore::deriveMasterKey()
{
	// Gather identity material that is:
	//   - unique to this machine
	//   - unique to this user
	//   - stable across reboots
	//
	// Replace this entire function with an OS-keychain fetch when you
	// want to store the master key in the platform vault.

	QStringList parts;

	// --- machine identity ---
#if defined(Q_OS_LINUX)
	{
		QFile f(QStringLiteral("/etc/machine-id"));
		if (f.open(QIODevice::ReadOnly))
			parts.append(QString::fromUtf8(f.readAll().trimmed()));
	}
	{
		QFile f(QStringLiteral("/var/lib/dbus/machine-id"));
		if (f.open(QIODevice::ReadOnly))
			parts.append(QString::fromUtf8(f.readAll().trimmed()));
	}
#elif defined(Q_OS_MACOS)
	{
		// kern.uuid is stable per macOS install
		char buf[64];
		size_t len = sizeof(buf);
		if (sysctlbyname("kern.uuid", buf, &len, nullptr, 0) == 0)
			parts.append(QString::fromLatin1(buf, static_cast<int>(len) - 1));
	}
#elif defined(Q_OS_WIN)
	{
		// MachineGuid is stable per Windows install
		QSettings reg(
			QStringLiteral("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Cryptography"),
			QSettings::NativeFormat);
		QString guid = reg.value(QStringLiteral("MachineGuid")).toString();
		if (!guid.isEmpty())
			parts.append(guid);
	}
#endif

	// --- user identity ---
	parts.append(QDir::homePath());

	// --- application identity (fixed salt — not secret, just unique to JASP) ---
	parts.append(QStringLiteral("JASP"));

	// If we couldn't gather anything, fall back to a process-unique random key.
	// Secrets won't survive a restart, but at least nothing crashes.
	if (parts.isEmpty() || (parts.size() == 2 && parts.last() == QStringLiteral("JASP"))) {
		Log::log() << "SecretStore: no machine identity available, using ephemeral key" << std::endl;
		unsigned char rnd[crypto_secretbox_KEYBYTES];
		randombytes_buf(rnd, sizeof rnd);
		return QByteArray(reinterpret_cast<const char *>(rnd), sizeof rnd);
	}

	const QByteArray input = parts.join(QLatin1Char(':')).toUtf8();

	unsigned char key[crypto_secretbox_KEYBYTES];
	crypto_generichash(key, sizeof key,
					   reinterpret_cast<const unsigned char *>(input.constData()),
					   input.size(),
					   kMasterKeySeed, sizeof(kMasterKeySeed)); // keyed BLAKE2b

	return QByteArray(reinterpret_cast<const char *>(key), sizeof key);
}

// =============================================================================
// Symmetric encryption (libsodium crypto_secretbox)
// =============================================================================

QByteArray SecretStore::encrypt(const QByteArray &plaintext, const QByteArray &key)
{
	if (key.isEmpty() || key.size() != static_cast<int>(crypto_secretbox_KEYBYTES))
		return QByteArray(); // no encryption available

	unsigned char nonce[crypto_secretbox_NONCEBYTES];
	randombytes_buf(nonce, sizeof nonce);

	QByteArray ciphertext(plaintext.size() + crypto_secretbox_MACBYTES, '\0');
	crypto_secretbox_easy(
		reinterpret_cast<unsigned char *>(ciphertext.data()),
		reinterpret_cast<const unsigned char *>(plaintext.constData()),
		plaintext.size(),
		nonce,
		reinterpret_cast<const unsigned char *>(key.constData()));

	// Prepend nonce → output blob = nonce || ciphertext
	QByteArray blob;
	blob.reserve(sizeof nonce + ciphertext.size());
	blob.append(reinterpret_cast<const char *>(nonce), sizeof nonce);
	blob.append(ciphertext);
	return blob;
}

QByteArray SecretStore::decrypt(const QByteArray &blob, const QByteArray &key)
{
	if (key.isEmpty() || key.size() != static_cast<int>(crypto_secretbox_KEYBYTES))
		return QByteArray();

	if (blob.size() < static_cast<int>(crypto_secretbox_NONCEBYTES + crypto_secretbox_MACBYTES))
		return QByteArray(); // too short to be valid

	const auto *nonce      = reinterpret_cast<const unsigned char *>(blob.constData());
	const auto *ciphertext = nonce + crypto_secretbox_NONCEBYTES;
	const int   cLen       = blob.size() - static_cast<int>(crypto_secretbox_NONCEBYTES);

	QByteArray plaintext(cLen - crypto_secretbox_MACBYTES, '\0');
	if (crypto_secretbox_open_easy(
			reinterpret_cast<unsigned char *>(plaintext.data()),
			ciphertext, cLen,
			nonce,
			reinterpret_cast<const unsigned char *>(key.constData())) != 0)
	{
		Log::log() << "SecretStore: decryption failed (tampered data or wrong key)" << std::endl;
		return QByteArray();
	}

	return plaintext;
}

// =============================================================================
// Settings I/O — thin wrappers around QSettings
// =============================================================================

QString SecretStore::readSetting(Settings::Type setting)
{
	return Settings::value(setting).toString();
}

void SecretStore::writeSetting(Settings::Type setting, const QString &value)
{
	Settings::setValue(setting, value);
}

void SecretStore::deleteSetting(Settings::Type setting)
{
	Settings::remove(setting);
}

// =============================================================================
// Migration — encrypt any leftover plaintext from the qtkeychain era
// =============================================================================

void SecretStore::migrateIfNeeded(Settings::Type setting)
{
	const QString raw = readSetting(setting);
	if (raw.isEmpty())
		return;

	// Already encrypted?  Encrypted blobs are base64-encoded binary
	// and always start with a nonce (24 bytes of random bytes). Plaintext
	// API keys start with "sk-" or similar.  If it looks like base64 and
	// is long enough, assume it's already migrated.
	if (raw.size() > 64 && !raw.startsWith(QLatin1String("sk-"))
		&& !raw.startsWith(QLatin1String("{"))) // JSON values
	{
		// Probably already base64 ciphertext — don't double-encrypt
		return;
	}

	// Looks like plaintext — encrypt it
	Log::log() << "SecretStore: migrating plaintext setting to encrypted storage" << std::endl;
	const QByteArray key = masterKey();
	if (key.isEmpty()) {
		Log::log() << "SecretStore: no master key, skipping migration" << std::endl;
		return;
	}

	const QByteArray encrypted = encrypt(raw.toUtf8(), key);
	if (!encrypted.isEmpty()) {
		writeSetting(setting, QString::fromLatin1(encrypted.toBase64()));
	}
}

// =============================================================================
// Public API
// =============================================================================

QString SecretStore::read(const QString & /*logicalKey*/, Settings::Type setting)
{
	const QByteArray key = masterKey();

	// --- no encryption available → plaintext fallback ---
	if (key.isEmpty())
		return readSetting(setting);

	// --- one-time migration from plaintext ---
	migrateIfNeeded(setting);

	// --- decrypt ---
	const QString stored = readSetting(setting);
	if (stored.isEmpty())
		return QString();

	const QByteArray blob = QByteArray::fromBase64(stored.toLatin1());
	const QByteArray plain = decrypt(blob, key);

	return QString::fromUtf8(plain);
}

void SecretStore::write(const QString & /*logicalKey*/, const QString &value, Settings::Type setting)
{
	const QByteArray key = masterKey();

	if (key.isEmpty()) {
		// No encryption — store as plaintext
		writeSetting(setting, value);
		return;
	}

	const QByteArray encrypted = encrypt(value.toUtf8(), key);
	if (encrypted.isEmpty()) {
		Log::log() << "SecretStore: encryption failed, storing as plaintext" << std::endl;
		writeSetting(setting, value);
		return;
	}

	writeSetting(setting, QString::fromLatin1(encrypted.toBase64()));
}

void SecretStore::remove(const QString & /*logicalKey*/, Settings::Type setting)
{
	deleteSetting(setting);
}

QString SecretStore::encryptValue(const QString &plaintext)
{
	const QByteArray key = masterKey();
	if (key.isEmpty())
		return plaintext;

	const QByteArray encrypted = encrypt(plaintext.toUtf8(), key);
	if (encrypted.isEmpty())
		return plaintext;

	return QString::fromLatin1(encrypted.toBase64());
}

QString SecretStore::decryptValue(const QString &ciphertextBase64)
{
	const QByteArray key = masterKey();
	if (key.isEmpty())
		return ciphertextBase64;

	const QByteArray blob = QByteArray::fromBase64(ciphertextBase64.toLatin1());
	const QByteArray plain = decrypt(blob, key);

	if (plain.isEmpty() && !ciphertextBase64.isEmpty())
		return ciphertextBase64; // might be legacy plaintext

	return QString::fromUtf8(plain);
}
