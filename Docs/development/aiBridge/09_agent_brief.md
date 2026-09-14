# Entra ID Authentication — Agent Brief

> **If you are an agent picking this up cold, read this file first, then
> [`08_entra_auth_plan.md`](08_entra_auth_plan.md).** That document is the plan;
> this one is the context you need to act on it.

**Paste-able prompt:** *"Read `Docs/development/aiBridge/09_agent_brief.md` and
`Docs/development/aiBridge/08_entra_auth_plan.md`, then start Phase 2 of the
Entra ID auth work."*

---

## 1. Mission

JASP's AI chat feature (`AiBridge`) currently authenticates to an OpenAI-compatible
endpoint with a static API key. Enterprise customers refuse to distribute static
keys to their users and want **Microsoft Entra ID** identity instead.

Add **Entra ID bearer-token auth** to the AI feature.

**This is multi-customer.** Nothing may be hard-coded to one tenant. Endpoint,
tenant and OAuth scope are per-provider configuration so one binary serves all
customers. Our own Entra app registration is multi-tenant; each customer's admin
consents once in their own tenant.

**Vendor hosts nothing.** No proxy, no server, no egress. Customer usage runs on
the customer's own Azure subscription.

---

## 2. Current status

| | |
|---|---|
| **Azure prerequisites** | ✅ Done (see §1b of the plan for all values) |
| **Code** | Phase 1 complete and verified. Phase 2 (browser sign-in) **written, not yet built or run** |
| **Next task** | **Build and run Phase 2**, then the UI (Phase 3) |

The publisher-verified multi-tenant app registration already exists, so no Azure
work is needed. Phase 1 was verified end to end against the shipped Azure preset
— see §6. **No third-party binaries are needed at any point** — read the plan §3b
before suggesting otherwise.

---

## 3. Design in one picture

```
AiBridge ──► TokenProvider (abstract)
                ├── ApiKeyTokenProvider       (static key — the default)
                └── BrowserTokenProvider      (system browser + loopback PKCE)
```

`AiBridge` must not know *how* a token is obtained. Providers own acquisition,
caching, refresh and sign-out.

**There is deliberately no WAM/broker backend.** It needs a closed binary whose
redistribution terms are unclear, and JASP Desktop is AGPL3+. The plan §3b has
the full research record — do not re-derive it, and do not add a backend that
needs shipping a third-party binary.

Key interface (full sketch in the plan, §3):

```cpp
class TokenProvider : public QObject {
    Q_OBJECT
public:
    virtual QString   authMode() const = 0;   // "apiKey" | "oidc" | "none"
    virtual void      ensureToken() = 0;      // async; never blocks
    virtual QString   token() const = 0;
    virtual bool      isValid() const = 0;    // usable right now
    virtual QDateTime expiresAt() const = 0;
    virtual QString   accountName() const = 0;
    virtual void      signOut() = 0;

signals:
    void tokenReady(const QString &token);
    void interactionRequired(const QString &message);
    void authFailed(const QString &error);
};
```

### The one non-obvious thing about the request path

A browser sign-in needs the event loop, so `AiBridge` cannot wait for a token.
`sendToAI()` / `testConnection()` take a **fast path** when the provider
`isValid()` (that is all of `apiKey`, unchanged), and otherwise **park the
request** in `m_pendingSend` / `m_pendingTest` and resume it from
`onTokenReady()`. If the user stops the stream meanwhile, `m_streaming` is false
and the parked request is dropped.

---

## 4. Codebase orientation

| File | What it does |
|---|---|
| `Desktop/ai/aiBridge.{h,cpp}` | HTTP + SSE streaming, tool-call loop. `configureTokenProvider()` picks a provider; `applyAuthHeader()` writes the auth header. |
| `Desktop/ai/auth/tokenprovider.h` | The auth abstraction `AiBridge` consumes. |
| `Desktop/ai/auth/apitokenprovider.{h,cpp}` | Static API key — the default backend. |
| `Desktop/ai/auth/browsertokenprovider.{h,cpp}` | System browser + loopback PKCE via QtNetworkAuth. |
| `Desktop/gui/aiconfigmodel.{h,cpp}` | Singleton holding provider/model config. Persists to `Settings::AI_USER_PROVIDERS` as a JSON blob. |
| `Desktop/utilities/secretstore.{h,cpp}` | libsodium encryption for API keys. |
| `Desktop/utilities/settings.h` | `Settings::Type` enum — the persistence key registry. |
| `Desktop/components/JASP/Widgets/FileMenu/PrefsAI.qml` | The AI preferences UI (Endpoint / API Key / Model fields). |
| `Resources/defaultProviders.json` | Shipped provider presets. |
| `Desktop/CMakeLists.txt` | `JASPDesktopLib` target + Qt link list. |

Useful facts:

- `AiBridge` no longer reads the API key directly. `configureTokenProvider()`
  selects a `TokenProvider` from `currentAuthMode()`/`currentAuthBackend()`,
  `authToken()` delegates to it, and `applyAuthHeader()` writes
  `authHeaderName(): authHeaderPrefix() + token`. In `apiKey` mode that is
  byte-for-byte the old `Authorization: Bearer <key>`.
- `BrowserTokenProvider` needs **`Qt::NetworkAuth`** (added to
  `Tools/CMake/Libraries.cmake` and the `JASPDesktopLib` link list). Qt is
  **6.11.2**, so PKCE `S256` is native — no hand-rolled challenge.
- `AIProviderEntry` and `ProviderOverrides` (in `aiconfigmodel.h`) are where new
  auth config belongs.
- `JASPDesktopLib` collects sources with `file(GLOB_RECURSE …)` — **re-run CMake
  after adding files.**

---

## 5. Decisions already made — don't relitigate

1. **One interface, multiple backends.** Not one provider per method.
2. **Browser first.** The system browser with a loopback redirect is the flow
   JASP ships, on every platform. Device code is the fallback (Phase 5).
3. **`apiKey` mode stays the default** and must behave *exactly* as today.
4. **Refresh tokens go to the OS vault** (DPAPI / Keychain / libsecret) — **never**
   into `SecretStore`, whose key is a hardcoded constant.
5. **Multi-customer by configuration**, not by build.
6. **No vendor-hosted proxy.**
7. **No third-party binaries.** Everything shipped must be free software with
   published source and a clear licence. See the plan §3b.

---

## 6. Phase 1 — ✅ done (reference)

Delivered:

1. `Desktop/ai/auth/tokenprovider.h` — the interface (`authMode()`, `ensureToken()`,
   `token()`, `isValid()`, `signOut()` plus `tokenReady` / `interactionRequired` /
   `authFailed`).
2. `Desktop/ai/auth/apitokenprovider.{h,cpp}` — wraps `currentApiKey()`.
3. `AIProviderEntry` + `ProviderOverrides` extended with the protocol-shaped auth
   fields (plan §4): `authMode`, `authAuthority`, `authScope`, `authClientId`,
   `authBackend`, `authHeaderName`, `authHeaderPrefix`.
4. `AIConfigModel` — `Q_PROPERTY`s, getters/setters, signals, wired into
   `loadUserData()` / `saveUserData()`, with back-compat reads for the pre-rename
   `entraTenant` / `entraScope` keys and the `entra` scheme value.
5. `Resources/defaultProviders.json` — the **"Azure OpenAI (Entra ID)"** preset.
6. `AiBridge` — routes through `configureTokenProvider()` / `applyAuthHeader()`;
   `apiKey` yields exactly the previous request bytes.

**Acceptance** — *builds clean; every existing provider behaves exactly as today* —
is met: an existing API-key provider (DeepSeek) still works, and the Azure preset
reports *"Sign-in is not available in this build yet."*, which proves shipped JSON
→ `AIConfigModel` → `AiBridge` routing before any OIDC backend exists.

**Next:** Phase 2. Phases 2–7 are in the plan.

---

## 7. Landmines

| Issue | Detail |
|---|---|
| **Synchronous token read** | ✅ Resolved. `sendToAI()`/`testConnection()` now take a fast path when the provider `isValid()` and otherwise park the request and resume in `onTokenReady()`. Never block waiting for a token — the browser flow needs the event loop. |
| **A browser sign-in must never use an embedded view** | The chat UI is a `WebEngineView`, and Conditional Access device-compliance fails inside embedded web views. `BrowserTokenProvider` opens the **system** browser via `QDesktopServices::openUrl`. |
| **The loopback redirect must say `localhost`** | Qt's default handler advertises `127.0.0.1`, which the app registration does not accept → `AADSTS50011`. `BrowserTokenProvider` sets `setCallbackHost("localhost")` and logs the resolved URI on every attempt. |
| **Bind the OAuth listener to loopback explicitly** | `QOAuthHttpServerReplyHandler::listen()` defaults to `QHostAddress::Any`, and its loopback fallback only fires for a *null* address — so calling `listen()` with no arguments binds `0.0.0.0`. That exposes the redirect endpoint to the LAN and makes Windows show a firewall prompt for a listener that only ever needs the local browser. `BrowserTokenProvider` passes `QHostAddress::LocalHost`. |
| **A closed binary is not an option here** | The Windows broker (`msalruntime`) is a proprietary binary from an internal Microsoft repo, with contradictory licence terms across channels, whose EULA §3(e) prohibits distribution — and JASP Desktop is AGPL3+. `azure-identity-cpp` does not help either: it has no interactive browser credential. Full record in the plan §3b. |
| **Don't add vendor-named auth config** | Auth config is generic on purpose: scheme (`authMode`) is a *protocol* (`apiKey` / `oidc` / `none`), and the other axes are `authAuthority`, `authScope`, `authClientId`, `authBackend`, `authHeaderName` / `authHeaderPrefix`. A new identity provider is a new `TokenProvider` **backend**, never new config fields. `normalizeAuthMode()` maps the legacy `entra` value to `oidc`, so don't reintroduce it as a stored value. |
| **Prompt logging (privacy)** | `aiBridge.cpp` ~L549–550 logs the **full request body on every request**, unconditionally. Enterprise research data would land in log files. Fix before any pilot (Phase 6). |
| **Debug dump default** | `m_debugDumpEnabled = true` writes request bodies to `<tempDir>/ai-request.json`. Default it off. |
| **`SecretStore` is obfuscation** | `deriveMasterKey()` uses a hardcoded `kMasterKeySeed` in the binary. Fine for an API key; **not** for a refresh token. |
| **Embedded WebView** | The chat UI is a `WebEngineView`. **Never** run OAuth inside it — Conditional Access device-compliance fails in embedded views. Use the system browser or WAM. |
| **`msalruntime.dll` packaging** | No longer relevant — no such binary is shipped (plan §3b). |
| **Scope strings** | Azure OpenAI: `https://cognitiveservices.azure.com/.default`. Newer Foundry endpoints also accept `https://ai.azure.com/.default`. |
| **RBAC role** | Customers must assign `Cognitive Services OpenAI User`. **Not** `Cognitive Services User` — that one includes `listkeys` and defeats the purpose. |

---

## 8. Environment notes

- **WSL is not installed on this machine**, so there is no Linux sandbox: `terminal`
  commands run in the host shell — **Git Bash** where installed, otherwise
  PowerShell/cmd. Write commands for that shell and expect host path conventions
  (`C:\...` or `/c/...`, not `/mnt/c/...`).
- **`git` is available** and the project root is
  `C:\Users\rdoff\work\pro\jasp-desktop`. Read-only git commands should still be
  run as `git --no-pager …` so nothing blocks on a pager.
- **Commit only when explicitly asked, and never push.**
- **Qt is 6.11.2, MSVC 2022, at `C:\Qt\6.11.2\msvc2022_64`** — and the Qt source
  tree is installed at `C:\Qt\6.11.2\Src`. That is worth knowing: the exact
  NetworkAuth API is verifiable there (`Src\qtnetworkauth\src\oauth\`), which
  beats guessing at members from memory.
- **No third-party binary is fetched at any point.** The `Desktop/ai/auth/msal/`
  idea is obsolete — see the plan §3b.

---

## 9. Still needed from the human

| For | What |
|---|---|
| Phase 2 | Build and run it — nothing in the auth path is validated until then |
| Phase 3 | UI decisions: where sign-in lives in `PrefsAI.qml`, and what to show when signed in |
| Phase 4 | Preference: OS vault directly (DPAPI/Keychain/libsecret) or a small dependency such as QtKeychain |
| Phase 7 | A free secondary Entra tenant for multi-tenant consent testing |

Per-customer questions to gather (plan §9): OS mix, Conditional Access device
requirements, gateway vs direct, per-user attribution, data residency/DPA.

---

## 10. Reference — Azure facts

All live values (tenant, client ID, redirect URIs, publisher verification) are in
**§1b of the plan**. The path we took to get publisher verification — including
the gotchas — is in the plan's **Appendix B**.
