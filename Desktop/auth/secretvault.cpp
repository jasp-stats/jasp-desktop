//
// SecretVault implementation — see header for the backend and policy rationale.
//

#include "secretvault.h"

#include "log.h"
#include "encryptedsettingsstore.h"
#include "utilities/settings.h"

#include <QCryptographicHash>
#include <QSettings>

// =============================================================================
// Fallback store — EncryptedSettingsStore inside QSettings
// =============================================================================

namespace {

QString settingsKey(const QString &key)
{
	return QStringLiteral("SecretVault/") + key;
}

QByteArray fallbackRead(const QString &key)
{
	const QString stored = Settings::getSettings()->value(settingsKey(key)).toString();
	if (stored.isEmpty())
		return {};
	return EncryptedSettingsStore::decryptValue(stored).toUtf8();
}

void fallbackWrite(const QString &key, const QByteArray &value)
{
	Settings::getSettings()->setValue(
		settingsKey(key), EncryptedSettingsStore::encryptValue(QString::fromUtf8(value)));
}

void fallbackRemove(const QString &key)
{
	Settings::getSettings()->remove(settingsKey(key));
}

} // namespace

// =============================================================================
// Windows — Credential Manager
// =============================================================================

#ifdef Q_OS_WIN
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#  include <wincred.h>

namespace {

const wchar_t *targetName(const QString &key)
{
	return reinterpret_cast<const wchar_t *>(key.utf16());
}

bool writeCredential(const QString &key, const QByteArray &value)
{
	if (value.size() > CRED_MAX_CREDENTIAL_BLOB_SIZE)
	{
		Log::log() << "SecretVault: value of " << key.size() << "-char key is " << value.size()
		           << " bytes, over the store's limit — not persisted to Credential Manager" << std::endl;
		return false;
	}

	CREDENTIALW cred = {};
	cred.Type               = CRED_TYPE_GENERIC;
	cred.TargetName         = const_cast<wchar_t *>(targetName(key));
	cred.Persist            = CRED_PERSIST_LOCAL_MACHINE;
	cred.CredentialBlobSize = DWORD(value.size());
	cred.CredentialBlob     = reinterpret_cast<LPBYTE>(const_cast<char *>(value.constData()));

	return CredWriteW(&cred, 0) != FALSE;
}

QByteArray readCredential(const QString &key)
{
	PCREDENTIALW cred = nullptr;
	if (CredReadW(targetName(key), CRED_TYPE_GENERIC, 0, &cred) == FALSE)
		return {};

	const QByteArray value(reinterpret_cast<const char *>(cred->CredentialBlob),
	                        int(cred->CredentialBlobSize));
	CredFree(cred);
	return value;
}

void deleteCredential(const QString &key)
{
	CredDeleteW(targetName(key), CRED_TYPE_GENERIC, 0);
}

} // namespace
#endif // Q_OS_WIN

// =============================================================================
// macOS — Keychain
// =============================================================================

#ifdef Q_OS_MACOS
#  include <Security/Security.h>

namespace {

static const CFStringRef kKeychainService = CFSTR("JASP");

void keychainRemove(const QString &key)
{
	CFStringRef account = key.toCFString();
	const void *keys[]  = { kSecClass, kSecAttrService, kSecAttrAccount };
	const void *vals[]  = { kSecClassGenericPassword, kKeychainService, account };
	CFDictionaryRef query = CFDictionaryCreate(nullptr, keys, vals, 3, nullptr, nullptr);
	SecItemDelete(query);
	CFRelease(query);
	CFRelease(account);
}

bool keychainWrite(const QString &key, const QByteArray &value)
{
	// SecItemAdd fails with errSecDuplicateItem if the item exists; replace
	// is simplest as delete-then-add.
	keychainRemove(key);

	CFStringRef account = key.toCFString();
	CFDataRef    data    = CFDataCreateWithBytesNoCopy(
		nullptr, reinterpret_cast<const UInt8 *>(value.constData()),
		CFIndex(value.size()), kCFAllocatorNull);

	const void *keys[]  = { kSecClass, kSecAttrService, kSecAttrAccount, kSecValueData };
	const void *vals[]  = { kSecClassGenericPassword, kKeychainService, account, data };
	CFDictionaryRef query = CFDictionaryCreate(nullptr, keys, vals, 4, nullptr, nullptr);

	const OSStatus status = SecItemAdd(query, nullptr);

	CFRelease(query);
	CFRelease(data);
	CFRelease(account);
	return status == errSecSuccess;
}

QByteArray keychainRead(const QString &key)
{
	CFStringRef account = key.toCFString();
	const void *keys[]  = { kSecClass, kSecAttrService, kSecAttrAccount, kSecReturnData };
	const void *vals[]  = { kSecClassGenericPassword, kKeychainService, account, kCFBooleanTrue };
	CFDictionaryRef query = CFDictionaryCreate(nullptr, keys, vals, 4, nullptr, nullptr);

	CFDataRef data = nullptr;
	const OSStatus status = SecItemCopyMatching(query, reinterpret_cast<CFTypeRef *>(&data));

	CFRelease(query);
	CFRelease(account);

	if (status != errSecSuccess || !data)
	{
		if (data) CFRelease(data);
		return {};
	}

	const QByteArray value(reinterpret_cast<const char *>(CFDataGetBytePtr(data)),
	                       int(CFDataGetLength(data)));
	CFRelease(data);
	return value;
}

} // namespace
#endif // Q_OS_MACOS

// =============================================================================
// Linux — Secret Service via libsecret, resolved at runtime
// =============================================================================

#ifdef Q_OS_LINUX
#  include <QLibrary>

namespace {

// ABI-compatible redeclarations of the libsecret/glib types we touch, so that
// nothing from glib is a build-time dependency. Attribute type 0 = string;
// flags 0 = none.
struct SecretSchemaAttribute { const char *name; int type; };
struct SecretSchema
{
	const char *name;
	int flags;
	SecretSchemaAttribute attributes[32];
};

using SecretPasswordStoreSync  = int    (*)(SecretSchema *, void *, const char *, const char *, const char *, void *, void **);
using SecretPasswordLookupSync = char *(*)(SecretSchema *, void *, void *, void **);
using SecretPasswordClearSync  = int    (*)(SecretSchema *, void *, void *, void **);
using SecretPasswordFree       = void   (*)(char *);
using GHashTableNewFn          = void *(*)(void *, void *);
using GHashTableInsertFn       = int    (*)(void *, void *, void *);
using GHashTableUnrefFn        = void   (*)(void *);

/// Loaded once; a missing library leaves every pointer null and available()
/// false, which is the whole point of dlopen-ing instead of linking.
struct SecretServiceLib
{
	QLibrary secret{ QStringLiteral("libsecret-1.so.0") };
	QLibrary glib{ QStringLiteral("libglib-2.0.so.0") };

	SecretPasswordStoreSync  store        = nullptr;
	SecretPasswordLookupSync lookup       = nullptr;
	SecretPasswordClearSync  clear        = nullptr;
	SecretPasswordFree       freePassword = nullptr;
	GHashTableNewFn          tableNew     = nullptr;
	GHashTableInsertFn       tableInsert  = nullptr;
	GHashTableUnrefFn        tableUnref   = nullptr;

	bool ok = false;

	SecretServiceLib()
	{
		if (!secret.load() || !glib.load())
			return;

		store        = reinterpret_cast<SecretPasswordStoreSync>(secret.resolve("secret_password_store_sync"));
		lookup       = reinterpret_cast<SecretPasswordLookupSync>(secret.resolve("secret_password_lookup_sync"));
		clear        = reinterpret_cast<SecretPasswordClearSync>(secret.resolve("secret_password_clear_sync"));
		freePassword = reinterpret_cast<SecretPasswordFree>(secret.resolve("secret_password_free"));
		tableNew     = reinterpret_cast<GHashTableNewFn>(glib.resolve("g_hash_table_new"));
		tableInsert  = reinterpret_cast<GHashTableInsertFn>(glib.resolve("g_hash_table_insert"));
		tableUnref   = reinterpret_cast<GHashTableUnrefFn>(glib.resolve("g_hash_table_unref"));

		ok = store && lookup && clear && freePassword && tableNew && tableInsert && tableUnref;
	}
};

const SecretServiceLib &secretService()
{
	static const SecretServiceLib instance;
	return instance;
}

SecretSchema makeSchema()
{
	SecretSchema schema = {};
	schema.name               = "org.jaspstats.JASP.Secret";
	schema.attributes[0].name = "jasp-key";
	schema.attributes[0].type = 0; // string
	return schema;
}

void *makeAttributes(const SecretServiceLib &lib, const char *keyUtf8)
{
	// libsecret marshals this table into a DBus dict, so the client-side hash
	// function has no effect on the wire format — which is why libsecret's own
	// examples pass NULL, NULL.
	void *table = lib.tableNew(nullptr, nullptr);
	if (!table)
		return nullptr;
	lib.tableInsert(table, const_cast<char *>("jasp-key"), const_cast<char *>(keyUtf8));
	return table;
}

bool secretServiceWrite(const QString &key, const QByteArray &value)
{
	const SecretServiceLib &lib = secretService();
	if (!lib.ok)
		return false;

	// The password API passes C strings: base64 keeps arbitrary blobs intact.
	const QByteArray password = value.toBase64();
	const QByteArray keyUtf8  = key.toUtf8();

	void *attributes = makeAttributes(lib, keyUtf8.constData());
	if (!attributes)
		return false;

	const SecretSchema schema = makeSchema();
	const bool ok = lib.store(const_cast<SecretSchema *>(&schema), attributes, "default",
	                          keyUtf8.constData(),   // the label shown by seahorse etc.
	                          password.constData(),
	                          nullptr, nullptr) != 0;
	lib.tableUnref(attributes);
	return ok;
}

QByteArray secretServiceRead(const QString &key)
{
	const SecretServiceLib &lib = secretService();
	if (!lib.ok)
		return {};

	const QByteArray keyUtf8 = key.toUtf8();
	void *attributes = makeAttributes(lib, keyUtf8.constData());
	if (!attributes)
		return {};

	const SecretSchema schema = makeSchema();
	char *password = lib.lookup(const_cast<SecretSchema *>(&schema), attributes, nullptr, nullptr);
	lib.tableUnref(attributes);
	if (!password)
		return {};

	const QByteArray value = QByteArray::fromBase64(password);
	lib.freePassword(password);
	return value;
}

void secretServiceRemove(const QString &key)
{
	const SecretServiceLib &lib = secretService();
	if (!lib.ok)
		return;

	const QByteArray keyUtf8 = key.toUtf8();
	void *attributes = makeAttributes(lib, keyUtf8.constData());
	if (!attributes)
		return;

	const SecretSchema schema = makeSchema();
	lib.clear(const_cast<SecretSchema *>(&schema), attributes, nullptr, nullptr);
	lib.tableUnref(attributes);
}

} // namespace
#endif // Q_OS_LINUX

// =============================================================================
// SecretVault
// =============================================================================

bool SecretVault::available()
{
#if defined(Q_OS_WIN) || defined(Q_OS_MACOS)
	return true;
#elif defined(Q_OS_LINUX)
	return secretService().ok;
#else
	return false;
#endif
}

QByteArray SecretVault::read(const QString &key)
{
	QByteArray value;
#if defined(Q_OS_WIN)
	value = readCredential(key);
#elif defined(Q_OS_MACOS)
	value = keychainRead(key);
#elif defined(Q_OS_LINUX)
	value = secretServiceRead(key);
#endif
	if (!value.isEmpty())
		return value;

	// Only degradable secrets can ever be in the fallback store — write()
	// with Degrade::Never refuses to put anything there — so consulting it
	// here can never surface a token-class secret.
	return fallbackRead(key);
}

bool SecretVault::write(const QString &key, const QByteArray &value, Degrade degrade)
{
	bool stored = false;
#if defined(Q_OS_WIN)
	stored = writeCredential(key, value);
#elif defined(Q_OS_MACOS)
	stored = keychainWrite(key, value);
#elif defined(Q_OS_LINUX)
	stored = secretServiceWrite(key, value);
#endif

	if (stored)
	{
		// A previous run may have degraded this key before a vault existed;
		// the vault copy wins from now on, so drop the stale one.
		fallbackRemove(key);
		return true;
	}

	if (degrade == Degrade::Never)
		return false;

	fallbackWrite(key, value);
	Log::log() << "SecretVault: no OS credential store available — stored in the "
	              "obfuscated settings fallback" << std::endl;
	return true;
}

void SecretVault::remove(const QString &key)
{
#if defined(Q_OS_WIN)
	deleteCredential(key);
#elif defined(Q_OS_MACOS)
	keychainRemove(key);
#elif defined(Q_OS_LINUX)
	secretServiceRemove(key);
#endif
	fallbackRemove(key);
}

QString SecretVault::key(const QStringList &readableParts, const QString &uniquePart)
{
	const QByteArray hash =
		QCryptographicHash::hash(uniquePart.toUtf8(), QCryptographicHash::Sha256).toHex();

	QStringList parts;
	parts << QStringLiteral("JASP") << readableParts << QString::fromLatin1(hash.left(24));
	return parts.join(QLatin1Char('/'));
}
