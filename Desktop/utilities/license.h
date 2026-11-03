#ifndef LICENSE_H
#define LICENSE_H

// Enterprise license-expiry check for JASP PRO builds. The whole feature
// compiles away unless both -DPRO and -DLICENSE_CHECK are defined (the same
// whole-file pattern as auth/browsertokenprovider.h): community builds, and
// PRO builds configured with CMake LICENSE_CHECK=OFF, carry none of this.
// The minting side lives in Tools/license/license.py — see its docstring
// for the blob layout and the key-handling rules.
#if defined(PRO) && defined(LICENSE_CHECK)

#include <QString>

/// Reads the signed license blob shipped in forced.json (or pushed through
/// group policy, which outranks it), verifies it, and decides whether this
/// install should nag the user that their enterprise license has expired.
///
/// Fail-closed by design: in release builds anything but a valid, enabled,
/// unexpired blob leads to the nag. That includes a missing blob (deleting
/// forced.json must not silence the check) and a corrupted one (writing junk
/// into user-writable HKCU policies must not either). Debug builds nag the
/// same way whenever a blob is present — so the dialog is testable — but
/// stay silent without one, keeping blob-less development popup-free.
class License
{
public:
	enum class State {
		Absent,		///< no blob found anywhere — fails closed in release builds
		Dormant,	///< valid blob with enabled:false — we decided this install never nags
		StaleToken,	///< valid blob minted before this build existed (leaked-blob defense)
		Expired,	///< valid blob, past expiresAt — the nag
		Invalid,	///< undecodable, bad signature or malformed payload — fails closed
		Valid		///< all good, stay quiet
	};

	/// Runs the whole pipeline; logs every decision to the JASP log so batch
	/// runs and support logs carry the license state too.
	static State state();

	/// Startup hook (MainWindow): logs the state in every run mode and shows
	/// the one-per-session expiry popup in interactive release builds.
	static void runStartupCheck(bool interactive);

#ifdef LICENSE_ENFORCEMENT
	/// main() calls this before argument parsing: when the license is anything
	/// but valid/dormant it demotes AppInfo::proMode(), so SSO sign-in and the
	/// batch data-sync command line behave like a community build for the rest
	/// of the session (a fresh blob via forced.json or policy restores them on
	/// the next launch). Debug builds demote only when a blob was actually
	/// deployed — or JASP_LICENSE_TEST_FAIL_CLOSED is set — the same carve-out
	/// as the popup.
	static void enforceProDemotion();
#endif

private:
	static void showExpiryPopup();
	static bool _popupShown;
};

#endif // defined(PRO) && defined(LICENSE_CHECK)
#endif // LICENSE_H
