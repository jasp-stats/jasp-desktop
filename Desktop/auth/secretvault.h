//
// SecretVault — named secrets in the platform credential store.
//
// The one door secrets go through. Backend per platform:
//   Windows — Credential Manager (CredWrite/CredRead; DPAPI-protected, and
//             visible in the Control Panel so users and admins can audit it)
//   macOS   — Keychain (Security.framework generic passwords)
//   Linux   — Secret Service (org.freedesktop.secrets) via libsecret, loaded
//             at *runtime* with QLibrary. Present on GNOME always, KDE with
//             Frameworks >= 5.97, KeePassXC when set as the service; absent
//             on keyring-less setups (headless, minimal WMs). Runtime loading
//             keeps a missing libsecret a clean "unavailable" state rather
//             than a packaging dependency.
//
// Degradation policy (Degrade, on write only):
//   ToEncryptedSettings (default) — no OS vault, or the vault write failed:
//             store via EncryptedSettingsStore inside QSettings. For secrets
//             the user typed and can revoke, such as API keys.
//   Never    — refuse. For credentials that mint tokens on their own (OIDC
//             refresh tokens): obfuscated storage would look like persistence
//             while being decryptable by anyone with the source. The caller
//             reports the failure; the session still works in memory.
//
// read()/remove() carry no policy on purpose: a Never-secret can never be in
// the fallback store, because write() refuses to put it there — and remove()
// clears both stores unconditionally.
//
// Values are opaque blobs; anything structured belongs to the caller.
//
// Do not add an encryption layer on top of the OS vaults: its key would ship
// in this binary, so it would stop neither offline attackers (the OS vault
// already encrypts, with a key the process never sees) nor code running as
// the user (which can read the blob and the key alike).
//

#ifndef SECRETVAULT_H
#define SECRETVAULT_H

#include <QByteArray>
#include <QString>
#include <QStringList>

class SecretVault
{
public:
	enum class Degrade
	{
		ToEncryptedSettings,
		Never
	};

	/// True when this platform has a real OS credential store JASP can use.
	/// Note on Linux this means the *client library* is present; the host may
	/// still lack a running Secret Service, in which case write() fails and
	/// the Degrade policy decides what happens.
	static bool available();

	/// The secret for this key, or empty. Tries the OS vault first, then the
	/// fallback store.
	static QByteArray read(const QString &key);

	/// False when the value could not be persisted at all — only possible
	/// with Degrade::Never.
	static bool write(const QString &key, const QByteArray &value,
	                  Degrade degrade = Degrade::ToEncryptedSettings);

	/// Clears the secret from both the OS vault and the fallback store.
	static void remove(const QString &key);

	/// Build a stable, store-safe key: "JASP/<parts…>/<short hash of unique>".
	/// The readable parts identify the secret in the OS's own UI; the hash
	/// keeps distinct secrets apart without putting values into the key.
	static QString key(const QStringList &readableParts, const QString &uniquePart);
};

#endif // SECRETVAULT_H
