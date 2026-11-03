//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

// Enterprise license-expiry check — see license.h for the contract and
// Tools/license/license.py for the minting side. Everything below compiles
// to nothing unless this is a PRO build with LICENSE_CHECK on.
#if defined(PRO) && defined(LICENSE_CHECK)

#include "license.h"

#include "licensekeys.h"
#include "settings.h"
#include "log.h"
#include "appinfo.h"
#include "utilities/qutils.h"
#include "utilities/messageforwarder.h"

#include <sodium.h>

#include <QCoreApplication>
#include <QDateTime>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QLocale>

namespace
{
constexpr char	kMagic[4]			= {'J', 'L', 'I', 'C'};
constexpr quint8	kVersion			= 1;
constexpr int	kStaleSlackDays		= 30;	// blobs minted before the build (minus slack) are stale
constexpr int	kMagicLen			= 4;
constexpr int	kHeaderLen			= kMagicLen + 1 + crypto_secretbox_NONCEBYTES;	// magic + version + nonce

QString stateName(License::State state)
{
	switch (state)
	{
	case License::State::Absent:		return "absent";
	case License::State::Dormant:		return "dormant";
	case License::State::StaleToken:	return "stale-token";
	case License::State::Expired:		return "expired";
	case License::State::Invalid:		return "invalid";
	case License::State::Valid:			return "valid";
	}
	return "unknown";
}

/// AppInfo::builddate is __DATE__ " " __TIME__ " (Netherlands)", i.e.
/// "Oct  7 2026 14:03:11 (Netherlands)". Returns an invalid QDateTime if the
/// compiler's format ever changes — the stale-token rule is then simply
/// skipped (it is a hardening measure, not the gate).
QDateTime parseBuildDate()
{
	QString raw		= tq(AppInfo::builddate);
	const int brace	= raw.indexOf(" (");
	if (brace > 0)
		raw = raw.left(brace);
	return QLocale::c().toDateTime(raw.simplified(), "MMM d yyyy hh:mm:ss");
}
} // namespace

bool License::_popupShown = false;

static License::State computeLicenseState()
{
	if (sodium_init() < 0)
	{
		Log::log() << "License: libsodium failed to initialize; treating license as invalid." << std::endl;
		return State::Invalid;
	}

	const QString setting = Settings::value(Settings::LICENSE).toString().trimmed();
	if (setting.isEmpty())
	{
		Log::log() << "License: no license blob found (forced.json / group policy); this build fails closed." << std::endl;
		return State::Absent;
	}

	const QByteArray raw = QByteArray::fromBase64(setting.toUtf8());

	// Enough bytes for magic + version + nonce + MAC + payload(>=1) + signature
	if (raw.size() < kHeaderLen + crypto_secretbox_MACBYTES + 1 + crypto_sign_BYTES
		|| memcmp(raw.constData(), kMagic, kMagicLen) != 0
		|| quint8(raw.at(kMagicLen)) != kVersion)
	{
		Log::log() << "License: blob is not a JLIC v1 blob; treating license as invalid." << std::endl;
		return State::Invalid;
	}

	// Lift the curtain: secretbox under the compiled-in obfuscation key. This
	// hides the payload from people reading forced.json or the registry; the
	// security gate is the signature check below, not this.
	const auto* nonce		= reinterpret_cast<const unsigned char*>(raw.constData() + kMagicLen + 1);
	const auto* ciphertext	= reinterpret_cast<const unsigned char*>(raw.constData() + kHeaderLen);
	const int ctLen			= raw.size() - kHeaderLen;

	QByteArray plain(ctLen - crypto_secretbox_MACBYTES, '\0');
	if (crypto_secretbox_open_easy(reinterpret_cast<unsigned char*>(plain.data()),
								   ciphertext, ctLen, nonce, jaspLicenseKObf) != 0)
	{
		Log::log() << "License: blob failed authentication/decryption; treating license as invalid." << std::endl;
		return State::Invalid;
	}

	// plain = payload JSON ‖ detached Ed25519 signature. The signature is the
	// gate: only the JASP-held private key can produce a blob that passes
	// here, no matter which settings tier delivered it.
	const QByteArray sig		= plain.right(crypto_sign_BYTES);
	const QByteArray payload	= plain.left(plain.size() - crypto_sign_BYTES);

	if (crypto_sign_verify_detached(reinterpret_cast<const unsigned char*>(sig.constData()),
									reinterpret_cast<const unsigned char*>(payload.constData()),
									payload.size(), jaspLicensePK) != 0)
	{
		Log::log() << "License: blob signature does not verify; treating license as invalid." << std::endl;
		return State::Invalid;
	}

	QJsonParseError jsonErr;
	const QJsonDocument doc = QJsonDocument::fromJson(payload, &jsonErr);
	if (jsonErr.error != QJsonParseError::NoError || !doc.isObject())
	{
		Log::log() << "License: payload is not a JSON object; treating license as invalid." << std::endl;
		return State::Invalid;
	}
	const QJsonObject object = doc.object();

	if (object.value("schema").toInt() != 1)
	{
		Log::log() << "License: unknown payload schema; treating license as invalid." << std::endl;
		return State::Invalid;
	}

	const QString serial = object.value("serial").toString();
	Log::log() << "License: verified blob " << serial.toStdString() << "." << std::endl;

	// enabled defaults to true when absent: the fail-closed direction.
	if (!object.value("enabled").toBool(true))
	{
		Log::log() << "License: check disabled by the blob itself (enabled:false)." << std::endl;
		return State::Dormant;
	}

	// Stale-token rule: a blob minted before this build shipped (minus slack)
	// does not count. A leaked enabled:false or extra-long blob then dies with
	// the next release, while normally minted blobs (issued around or after
	// the build) and later GPO extensions keep working.
	const QDateTime issued = QDateTime::fromString(object.value("issuedAt").toString(), Qt::ISODateWithMs);
	const QDateTime built = parseBuildDate();
	if (!issued.isValid())
	{
		Log::log() << "License: payload has no valid issuedAt; treating license as invalid." << std::endl;
		return State::Invalid;
	}
	if (built.isValid() && issued < built.addDays(-kStaleSlackDays))
	{
		Log::log() << "License: blob predates this build by more than " << kStaleSlackDays << " days; stale token." << std::endl;
		return State::StaleToken;
	}

	const QString expiresString = object.value("expiresAt").toString();
	if (expiresString.isEmpty() || expiresString == "null")
	{
		Log::log() << "License: blob never expires; staying quiet." << std::endl;
		return State::Valid;
	}

	const QDateTime expires = QDateTime::fromString(expiresString, Qt::ISODateWithMs);
	if (!expires.isValid())
	{
		Log::log() << "License: payload has no valid expiresAt; treating license as invalid." << std::endl;
		return State::Invalid;
	}

	if (QDateTime::currentDateTimeUtc() >= expires)
	{
		Log::log() << "License: expired." << std::endl;
		return State::Expired;
	}

	Log::log() << "License: valid until " << expires.toString(Qt::ISODateWithMs).toStdString() << "." << std::endl;
	return State::Valid;
}

License::State License::state()
{
	// Frozen per process: forced.json and policy cannot meaningfully change
	// mid-run (a fresh blob takes effect on the next launch), and the state is
	// consulted from main() before argument parsing as well as at startup.
	static const State result = computeLicenseState();
	return result;
}

#ifdef LICENSE_ENFORCEMENT
void License::enforceProDemotion()
{
	const State result = state();

	if (result == State::Valid || result == State::Dormant)
		return;

#ifndef NDEBUG
	// Debug builds keep their PRO features when no blob was deployed at all
	// (same carve-out as the popup); set JASP_LICENSE_TEST_FAIL_CLOSED=1 to
	// see the release behavior without a release build.
	if (result == State::Absent && qEnvironmentVariableIsEmpty("JASP_LICENSE_TEST_FAIL_CLOSED"))
	{
		Log::log() << "License: no blob deployed; keeping PRO features in this debug session." << std::endl;
		return;
	}
#endif

	Log::log() << "License: PRO features are disabled for this session (" << stateName(result) << ")." << std::endl;
	AppInfo::setProMode(false);
}
#endif // LICENSE_ENFORCEMENT

void License::runStartupCheck(bool interactive)
{
	const State result = state();
	Log::log() << "License: state is " << stateName(result) << "." << std::endl;

	if (!interactive)
		return; // batch/headless runs: the state is in the log, nothing to pop up

	if (result == State::Valid || result == State::Dormant)
		return;

#ifdef NDEBUG
	const bool nag = true;
#else
	// Debug builds pop up whenever there is actually a license to check — so
	// the dialog is testable by dropping any blob (e.g. Tools/license/examples/
	// forced-expired.json) next to the executable — but stay silent when no
	// blob exists at all: the absent-case nag is for shipped installs, and a
	// developer without forced.json must not be nagged every run. Setting
	// JASP_LICENSE_TEST_FAIL_CLOSED=1 makes a debug build nag exactly like a
	// release one, so that rule is demonstrable without a release kit.
	const bool nag = result != State::Absent
				   || !qEnvironmentVariableIsEmpty("JASP_LICENSE_TEST_FAIL_CLOSED");
#endif
	if (!nag || _popupShown)
		return;
	_popupShown = true;

	showExpiryPopup();
}

void License::showExpiryPopup()
{
	// Deliberately no dates in the text: they are nobody's business but ours
	// (that is what the curtain key is for) and they would only go stale.
	MessageForwarder::showWarning(
		QCoreApplication::translate("License", "JASP Enterprise"),
#ifdef LICENSE_ENFORCEMENT
		QCoreApplication::translate("License", "This enterprise version of JASP has expired and its PRO features, such as signing in with a work account and automated data synchronization, have been disabled. Please contact your provider for the latest version."));
#else
		QCoreApplication::translate("License", "This enterprise version of JASP has expired. Please contact your provider for the latest version."));
#endif
}

#endif // defined(PRO) && defined(LICENSE_CHECK)
