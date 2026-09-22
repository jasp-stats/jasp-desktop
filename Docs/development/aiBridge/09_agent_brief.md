# Entra ID Authentication — Agent Brief

> **If you are an agent picking this up cold, read this file first, then
> [`08_entra_auth_plan.md`](08_entra_auth_plan.md).** That document is the plan and the
> research record; this one is the context you need to act on it.

**Paste-able prompt:** *"Read `Docs/development/aiBridge/09_agent_brief.md` and
`Docs/development/aiBridge/08_entra_auth_plan.md`. Then commit the pending work described in
§2 of the brief, and continue with the next steps in §8."*

---

## 1. Mission

JASP's AI chat feature (`AiBridge`) authenticates to an OpenAI-compatible endpoint with a
static API key. Enterprise customers refuse to distribute static keys to their users and want
**Microsoft Entra ID** identity instead.

Add **Entra ID bearer-token auth** to the AI feature.

**This is multi-customer.** Nothing may be hard-coded to one tenant. Endpoint, tenant and
OAuth scope are per-provider configuration so one binary serves all customers.

**Vendor hosts nothing.** No proxy, no server, no egress. Customer usage runs on the
customer's own Azure subscription.

---

## 2. Status

| Phase | State |
|---|---|
| **0 — Azure prerequisites** | ✅ Done |
| **1 — TokenProvider abstraction + generic config** | ✅ Done, committed |
| **2 — Browser sign-in backend** | ✅ **Done and verified end-to-end against live Azure** |
| **3 — UI (`PrefsAI.qml`)** | 🟡 **Tab skeleton built, in the working tree** — `aiAuthMode` setting + `AIConfigModel.authMode` (two-way consistency with the active provider), TabView with API-key / Sign-in tabs, sign-in card with four states, auth passthroughs on `AiBridge`. Not yet run — QML lints clean against baseline |
| **4 — Refresh-token persistence (OS vault)** | 🟡 **Windows done in the working tree** — `SecretVault` (Credential Manager) + silent renewal; macOS/Linux stay in-memory until Keychain/libsecret |
| **5 — Device-code fallback** | ❌ Not started |
| **6 — Privacy hardening** | 🟡 In the working tree, uncommitted — see §2 |
| **7 — Test matrix / second tenant** | ❌ Not started |
| **Gateway support (shapes 1–3)** | ✅ **Shape 2 verified end-to-end against live APIM, 2026-09-21** — see `10_apim_test_checklist.md`. Shape 3 still untested. |

**Commits on `development`:** `a377a2743` (build on VS 2026), `db64f2a57` (TokenProvider +
generic auth config), `ccd395773` (browser OIDC sign-in), `8f07714ef` (**temporary checkpoint**:
SecretVault + API-key migration + silent renewal, tabbed AI settings with per-mode provider
memory, APIM record — meant to be split into logical commits before merging).

### Committed as one temporary checkpoint — split before merging

| File | What changed |
|---|---|
| `Desktop/ai/aiBridge.cpp` | HTTP error **bodies** are surfaced instead of discarded; `onReplyError()` stays quiet when the server already answered (so Azure's message is not overwritten by a generic one) |
| `Desktop/auth/browsertokenprovider.cpp` | error handling, percent-decoding of provider errors, `oid`/`tid` added to the token log line |
| `Desktop/auth/browsertokenprovider.h` | `m_serverError` replaces the dead manual-refresh field |
| `Docs/development/aiBridge/08_entra_auth_plan.md` | substantially expanded — see below |
| `Desktop/ai/aiBridge.h` | `m_debugDumpEnabled` default `true` → `false` (Phase 6) |
| `Desktop/ai/aiBridge.cpp` | the unconditional `REQUEST BODY` log is now gated behind `m_verboseLogging`; disabling the dump deletes the stale `<tempDir>/ai-request.json` instead of leaving the last request body on disk |
| `Docs/development/aiBridge/10_apim_test_checklist.md` | **rewritten** — no longer a build checklist but the verified record of the APIM run, with the policies, the diagnostics that worked, and the landmines |
| `Desktop/auth/secretvault.{h,cpp}` | **new** — the one door for secrets. Backends: Credential Manager (Windows), Keychain (macOS), Secret Service via runtime-loaded libsecret (Linux). `Degrade` on write: `ToEncryptedSettings` (default, for API keys) or `Never` (refresh tokens → hard failure, logged). macOS/Linux backends are **written but not compiled** — no toolchain on the Windows box |
| `Desktop/utilities/encryptedsettingsstore.{h,cpp}` | **renamed from `secretstore`** — same class content, honest header comment; its role is now SecretVault's obfuscated fallback for degradable secrets only |
| `Desktop/auth/browsertokenprovider.{h,cpp}` | refresh token persisted with `Degrade::Never`; a failed write logs "sign-in works but will not persist"; silent renewal on startup; rejected token dropped from the vault with interactive fallback |
| `Desktop/gui/aiconfigmodel.{h,cpp}` | **API keys migrated onto SecretVault** (`JASP/AI/provider/<hash>`, default degrade). Legacy keys inside `aiUserProviders` are moved to the vault on first load and the JSON field is dropped. `resetToDefaults()` clears vault entries so a reset truly resets |
| `Desktop/auth/encryptedsettingsstore.{h,cpp}` | **moved into `ai/auth/`, vault-internal** — no include outside `secretvault.cpp` and the one-time migration reader; the dead typed `read/write/remove(Settings::Type)` API was stripped |
| `Desktop/ai/aiBridge.{h,cpp}` + `ChatWindow.qml` | the intro greeting is no longer prefetched by config signals at startup — the chat window sends it when it becomes visible, and only into an empty conversation |
| `PrefsAI.qml` + `aiBridge.{h,cpp}` + `aiconfigmodel.{h,cpp}` + `settings.{h,cpp}` | **tabbed AI settings**: `aiAuthMode` (`apiKey` default / `oidc`) drives a TabView with two bodies — the old form as the API-key tab, and a sign-in tab (endpoint, deployment, sign-in card with signed-in/waiting/error states, advanced authority/scope/clientId). Mode ↔ provider consistency is two-way: picking the Entra provider flips the tab and vice versa. `providerValues` items now carry `authMode`. `aiAuthMode` rides the existing GPO precedence, so a policy can pin the tab |

**Do not commit:** `Desktop/utilities/allhelp.{cpp,h}` (modified per `git status` but the diff
is empty — a CRLF artifact), `.qtcreator/`, and the generated
`Desktop/data/importers/{rdata/rdata.h,readstat/readstat.h}`.

The plan doc's §3c is now the reference for deployment shapes, gateway specifics, the Claude
Desktop parity target, and the managed-configuration findings. Read it before designing
anything in this area.

---

## 3. What "verified" means

Phase 2 is proven, not assumed. The confirming log line was:

```
BrowserTokenProvider: token acquired (aud=https://cognitiveservices.azure.com scp=user_impersonation)
AiBridge: POST #1 to https://test888.openai.azure.com/openai/v1/chat/completions
```

…followed by a real model reply from Azure OpenAI. So: system browser → Entra sign-in →
loopback PKCE → token exchange → bearer token → model response all work.

`test888` is a live Azure OpenAI resource used for this. It bills per token
(`Data Zone Standard (US)`), and the signed-in user has `Cognitive Services OpenAI User` on
it. Delete it when finished testing, or leave it — idle cost is zero.

---

## 4. Design in one picture

```
AiBridge ──► TokenProvider (abstract)
                ├── ApiKeyTokenProvider       (static key — the default)
                └── BrowserTokenProvider      (system browser + loopback PKCE)
```

`AiBridge` must not know *how* a token is obtained. Providers own acquisition, caching,
refresh and sign-out.

**There is deliberately no WAM/broker backend.** It needs a closed binary whose redistribution
terms are unclear, and JASP Desktop is AGPL3+. The plan §3b has the full research record — do
not re-derive it, and do not add a backend that needs shipping a third-party binary. Note
§3c's caveat: Anthropic *do* ship the broker and justify it on data-path grounds, so this
decision is worth re-examining with Microsoft rather than treating as settled forever.

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

A browser sign-in needs the event loop, so `AiBridge` cannot wait for a token. `sendToAI()` /
`testConnection()` take a **fast path** when the provider `isValid()` (that is all of `apiKey`,
unchanged), and otherwise **park the request** in `m_pendingSend` / `m_pendingTest` and resume
it from `onTokenReady()`. If the user stops the stream meanwhile, `m_streaming` is false and
the parked request is dropped.

---

## 5. Deployment shapes — what we support

Full detail in plan §3c. Summary:

| # | Shape | Credential | Secret on the client? | Our support |
|---|---|---|---|---|
| 1 | Direct Azure OpenAI + Entra | user token | **no** | ✅ **working** |
| 2 | Gateway validating the same audience | user token | **no** | ✅ **zero code** — endpoint change |
| 3 | Gateway publishing its own scope | user token | **no** | ✅ **zero code** — `authScope` change |
| 4 | Gateway requiring a subscription key too | token + key | **yes** | ❌ needs work; **consider whether to** |
| 5 | Key-only gateway | key | **yes** | ✅ works today (`authMode: apiKey`) |
| 6 | Key in a custom header | key | **yes** | ✅ works today (`authHeaderName`/`authHeaderPrefix`) |

**The decision the human made:** support **1–3**, browser-based sign-in only. Shapes 4–6 are
either already working or deliberately out of scope.

Three findings that matter when you design here:

- **Shape 4 is not the target.** Microsoft's own reference implementation
  (`microsoft/AzureOpenAI-with-APIM`) keys its token counters on *forwarded identity claims*
  when no APIM subscription is present, and advises moving off subscription keys beyond
  development. Per-user governance therefore does **not** require a key. And Claude Desktop
  cannot do token-plus-key either — its header map is non-secret by policy. Do not build this
  without a customer who actually needs it.
- **Bring-your-own app registration is the enterprise-friendly path.** Claude Desktop has the
  *customer* register a single-tenant app and paste its client ID into the tool's config, so
  **no vendor application ever enters the customer's tenant**. That eliminates the entire
  `AADSTS650052`/`700016` class of problems and the vendor-trust review. **We already support
  it** — `authClientId` is a per-provider field. Support both and document this one.
- **Managed configuration already exists in JASP.** See §7.

---

### What is verified, and what is only reasoned

Keep this straight, because the shapes above were mostly derived from Microsoft's and vendors'
documentation. There are now exactly two live tests: the direct Azure OpenAI path, and one
APIM gateway (2026-09-21). Nothing else has been exercised.

| Claim | Status |
|---|---|
| Direct Azure OpenAI + Entra works end to end | ✅ **tested** — token, role, real model reply |
| A tenant without an Azure subscription cannot consent at all | ✅ **tested** — the raw error |
| Owner alone does not grant model access | ✅ **tested** — `401 PermissionDenied` |
| The `AADSTS650057 → 650052 → 700016` chain and its fixes | ✅ **tested** |
| Shape 2 works with an endpoint change only | ✅ **tested** against live APIM — auth, quota and managed identity all exercised. Caveat: the endpoint change was *not* the only change. The import's path surface needed a `rewrite-uri`. See `10_apim_test_checklist.md`. |
| Shape 3 works via a foreign scope, and dynamic consent behaves for a cross-tenant client | ⚠️ **documented, not tested** — the least certain claim in this document |
| `id_token` mode works against LiteLLM/APIM as their docs describe | ⚠️ **documented, not tested** |
| The path-shape mismatch bites in practice | ✅ **tested** — the import publishes `/chat/completions`, Azure OpenAI serves `/openai/v1/chat/completions`, and the mismatch consumed most of a debugging session |
| Per-user quota buckets are actually keyed on `oid` | ⚠️ **unproven.** The limiter is proven to run; that it is not silently falling back to `"anonymous"` needs two accounts |
| LiteLLM's JWT auth is Enterprise-licensed | ✅ stated unambiguously by the vendor |
| **A gateway has been stood up and accepted traffic** | ✅ APIM Developer, 2026-09-21 — the first in this project's history |

**Cheapest way to close the gap, in order:**

1. **Same-tenant scope test** — free, ~10 minutes, no gateway. Register an app in our *own*
   tenant, expose a scope, point `authScope` at it, and confirm the returned token's `aud` is
   that app. Validates the scope→audience mechanism on its own.
2. ~~**A real gateway.**~~ ✅ **Done, 2026-09-21** — APIM Developer, shape 2. A local LiteLLM
   remains interesting only because its JWT auth is Enterprise-licensed, which is itself the
   finding.
3. **A second tenant** for genuine cross-tenant consent — see §8 step 8.

---

## 6. Codebase orientation

| File | What it does |
|---|---|
| `Desktop/ai/aiBridge.{h,cpp}` | HTTP + SSE streaming, tool-call loop. `configureTokenProvider()` picks a provider; `applyAuthHeader()` writes the auth header; `onReplyFinished()`/`onReplyError()` handle failures. |
| `Desktop/auth/tokenprovider.h` | The auth abstraction `AiBridge` consumes. |
| `Desktop/auth/apitokenprovider.{h,cpp}` | Static API key — the default backend. |
| `Desktop/auth/browsertokenprovider.{h,cpp}` | System browser + loopback PKCE via QtNetworkAuth. |
| `Desktop/gui/aiconfigmodel.{h,cpp}` | Singleton holding provider/model config. Persists to `Settings::AI_USER_PROVIDERS` as a JSON blob. `loadUserData()` is where the config is read (`aiconfigmodel.cpp:1078`). |
| `Desktop/utilities/secretstore.{h,cpp}` | libsodium encryption for API keys. |
| `Desktop/utilities/settings.{h,cpp}` | `Settings::Type` registry **and the policy precedence chain** (`value()`): machine GPO → user GPO → INI → legacy registry → defaults. |
| `Desktop/gui/jaspConfiguration/` | `conf.toml` local config + remote-config fetch, with a parser factory. Carries modules/analysis options/constants today, **not** AI settings. |
| `Desktop/components/JASP/Widgets/FileMenu/PrefsAI.qml` | The AI preferences UI. |
| `Resources/defaultProviders.json` | Shipped provider presets. The Azure one is `f2b9a4d1-…`. |
| `Desktop/CMakeLists.txt` | `JASPDesktopLib` target + Qt link list. |

Useful facts:

- `AiBridge` no longer reads the API key directly. `configureTokenProvider()` selects a
  `TokenProvider`, `authToken()` delegates to it, and `applyAuthHeader()` writes
  `authHeaderName(): authHeaderPrefix() + token`. In `apiKey` mode that is byte-for-byte the
  old `Authorization: Bearer <key>`.
- The shipped Azure preset uses the **v1** endpoint surface
  (`https://<res>.openai.azure.com/openai/v1/chat/completions`) with **empty `extraParams`** —
  the v1 API needs **no `api-version`**. Its `model` field is the **deployment name**, not the
  model's name (the preset ships the literal placeholder `your-deployment-name`).
- Its `authAuthority` is **`organizations`**, so sign-in lands in whichever tenant owns the
  account used. Correct for shipping; easy to misdiagnose while testing.
- `BrowserTokenProvider` needs **`Qt::NetworkAuth`** (already added to
  `Tools/CMake/Libraries.cmake` and the link list). Qt is **6.11.2**, so PKCE `S256` is native.
- `AIProviderEntry` and `ProviderOverrides` (in `aiconfigmodel.h`) are where new auth config
  belongs.
- `JASPDesktopLib` collects sources with `file(GLOB_RECURSE …)` — **re-run CMake after adding
  files.**
- **Everything needed for Claude-parity is already in Qt**, file:line-verified in plan §3c:
  `QAbstractOAuth2::idToken()` (`qabstractoauth2.h:163`), `setAuthorizationUrl`/
  `setTokenUrl` (`qabstractoauth.h:100`, `qabstractoauth2.h:166`),
  `QOAuthHttpServerReplyHandler(quint16 port, …)` (`qoauthhttpserverreplyhandler.h:29`),
  `setCallbackPath()` (`:37`), `expirationAt()` (`qabstractoauth2.h:146`).

---

## 7. Decisions already made — don't relitigate

1. **One interface, multiple backends.** Not one provider per method.
2. **Browser first.** The system browser with a loopback redirect is the flow JASP ships, on
   every platform. Device code is the fallback (Phase 5).
3. **`apiKey` mode stays the default** and must behave *exactly* as today.
4. **Refresh tokens go to the OS vault** (DPAPI / Keychain / libsecret) — **never** into
   `SecretStore`, whose key is a hardcoded constant.
5. **Multi-customer by configuration**, not by build.
6. **No vendor-hosted proxy.**
7. **No third-party binaries.** Everything shipped must be free software with published source
   and a clear licence. See plan §3b.
8. **Support deployment shapes 1–3**, browser-based sign-in. (Human decision.)
9. **Managed configuration is not a gap** — `Settings::value()` already implements the
   machine-GPO-over-user-settings precedence, and `AI_USER_PROVIDERS` flows through it, so an
   administrator can push the whole AI provider configuration today with no new code. Do not
   build a parallel mechanism. Caveats (GPO is Windows-only; policy overrides rather than
   merges; never put a shared key in `HKLM`) are in plan §3c.

---

## 8. Next steps, in order

**1. Split the temporary checkpoint `8f07714ef` into logical commits** (§2 has the
inventory): error-bodies/privacy, vault + migration, AI settings tabs + intro lifecycle,
docs.

**2. Finish Phase 6 — privacy hardening.** *Mostly done in the working tree* (§2): the
unconditional request-body log is gated behind `m_verboseLogging`, and `m_debugDumpEnabled` now
defaults off. **Still open:** sign-out/revocation, shared-machine handling, and temp-file
cleanup beyond the stale-dump removal. `SecretStore` remains obfuscation, not confidentiality —
it must not hold a refresh token (Phase 4).

**2b. Prove the quota buckets are per-user** (`10_apim_test_checklist.md`, "What is not proven
yet"). Two accounts against the gateway; hammer as one, confirm the other still works. This is
the difference between governance and a global cap that merely exists. Cheap, and it is the
last unverified claim in shape 2.

**3. Phase 3 — the UI** (`PrefsAI.qml`). Nothing is usable without it. It needs to express:
sign-in method (`authMode`), authority, scope, client ID, endpoint, model, plus the new fields
from step 4 — and to hide the API-key field when `authMode` is `oidc`. Show the signed-in
account and an expiry/refresh indicator.

**4. Claude-parity config support** (plan §3c has the full table). In rough value order:
- **`bearerTokenType`** — send the ID token instead of the access token. This is Claude's
  default and it materially reduces customer setup: an id_token's `aud` is the client's own
  ID, so a gateway needs only JWKS URL + audience and **no customer-published scope**. Direct
  Azure OpenAI must stay `access_token` (`aud` must be `cognitiveservices.azure.com`), so it is
  a per-deployment setting.
- **A custom-headers map** (non-secret routing headers only — match Claude's policy, which
  forbids credentials there).
- **`appendOfflineAccess`** — we currently *always* append `offline_access`; Claude
  deliberately does not when `scopes` is set explicitly in id_token mode (OIDC Core §11). The
  failure mode is invisible until a session outlives an hour.
- **`redirectPort`** and explicit `authorizationUrl`/`tokenUrl` — for Okta (exact-port match)
  and IdPs that serve no discovery document.

**5. Customer-facing deployment guide, then a PDF.** `10_apim_test_checklist.md` is now the
verified transcript to write this from — it holds the working policies, the values that are ours
versus the customer's, and the diagnostics that resolved each failure. Content belongs in
`Docs/user-guide/`,
following the existing convention there (markdown + PNG screenshots — see `logging-howto.md`).
`Docs/development/aiBridge/render_mermaid.py` exists if diagrams are wanted. **There is no
markdown→PDF pipeline in the repo** — the PDF step is a new decision (pandoc, a build step, or
external). Cover shapes 1–3, and both the shared-app and bring-your-own-app paths.

**6. Managed configuration for AI settings.** GPO works today for the whole
`aiUserProviders` blob; the cross-platform gap is `conf.toml`, which does not yet carry AI
settings. Decide merge vs override semantics first.

**7. Phase 4 (OS vault), then Phase 5 (device code).** Until Phase 4, users sign in once per
JASP run.

**8. Verify dynamic consent for shape 3** against a real second tenant — the one thing in the
plan explicitly marked unverified. Until then do not promise a customer that shape 3 is
drop-in.

---

## 9. Landmines

The high-value section. Most of these cost real time.

| Issue | Detail |
|---|---|
| **A browser sign-in must never use an embedded view** | The chat UI is a `WebEngineView`, and Conditional Access device-compliance fails in embedded web views. `BrowserTokenProvider` opens the **system** browser via `QDesktopServices::openUrl`. |
| **Bind the OAuth listener to loopback explicitly** | `QOAuthHttpServerReplyHandler::listen()` defaults to `QHostAddress::Any`, and its loopback fallback only fires for a *null* address — so calling `listen()` with no arguments binds `0.0.0.0`, exposing the redirect endpoint to the LAN and triggering a Windows Firewall prompt. `BrowserTokenProvider` passes `QHostAddress::LocalHost`. |
| **The loopback redirect must say `localhost`** | Qt's default handler advertises `127.0.0.1`, which our registration does not accept → `AADSTS50011`. `setCallbackHost("localhost")` is required. **Note:** Claude Desktop's docs require the opposite (`127.0.0.1/callback`, with the path) and say `localhost` is wrong. Both may be valid; **ours is verified working — do not "fix" it to match theirs without re-registering and re-testing.** |
| **A tenant must own an Azure subscription for consent to be possible at all** | Service principals for Microsoft's APIs are provisioned per the products a tenant owns. A bare Entra tenant has **none** — not even Microsoft Graph — and admin consent fails with *"Your organization does not have a subscription (or service principal)…"*. This consumed most of a day. A customer using Azure OpenAI always has a subscription, so **this is a test-environment artifact, not a product problem.** |
| **Admin consent was never required** | An earlier version of the plan insisted on it and sent us down a long wrong path. For one person signing in, **user consent is sufficient**. Admin consent is only a convenience for *other* users. |
| **The portal permission picker cannot offer an API the tenant has no service principal for** | *"APIs my organization uses"* lists only APIs with an SP in the tenant. Searching by name **or** by resource ID finds nothing on an empty tenant. Add the permission through the **app manifest** (`requiredResourceAccess`) instead. |
| **Owner ≠ data-plane access** | Being subscription Owner grants nothing on the model. `Cognitive Services OpenAI User` on the resource is required — via a **group**, one assignment. |
| **A missing RBAC role returns HTTP 401, not 403** | Azure replies `401 PermissionDenied: Principal does not have access to API/Operation`. Qt maps 401 to `AuthenticationRequiredError`, which the old message text rendered as *"check your API key"* — actively misleading. Also, role assignments take **minutes** to propagate; retrying immediately looks like failure. |
| **HTTP error bodies were being thrown away** | `onReplyFinished()` read the response body into `body` and then discarded it in the error branch, while `onReplyError()` emitted a generic message that overwrote anything useful. Fixed in the uncommitted work. If you see a generic auth error with no server detail, check this has not regressed. |
| **`authAuthority: organizations` makes tenant confusion easy** | Sign-in lands in whichever tenant owns the account used, and consent must be granted **there**. Entra's error messages name the *target* tenant while the app registration lives in the vendor tenant. The token log now prints `oid` and `tid` for exactly this reason — a token for the wrong identity produces identical symptoms to a missing role. |
| **`AADSTS650057` → `650052` → `700016` is a chain, not three problems** | `650057` = permission absent from `requiredResourceAccess`. `650052` = tenant has no service principal for the **resource** app. `700016` = the app was not found in the tenant named in the URL (we sent an `adminconsent` URL built from the *sign-in* error's tenant GUID — a wrong-tenant redirect on our side). Consent from inside the app's own directory in the portal avoids the guessing. |
| **`Data Zone Standard` bills per token; `Provisioned` bills hourly** | Idle cost is zero on the former. Do not create a PTU deployment for testing. |
| **Foundry vs Azure OpenAI scopes differ** | Azure OpenAI resource: `https://cognitiveservices.azure.com/.default`. Microsoft Foundry project endpoint: `https://ai.azure.com/.default`. |
| **Prompt logging (privacy)** | `aiBridge.cpp` logs the full request body unconditionally, and `m_debugDumpEnabled` defaults on. Fix before any pilot (Phase 6). |
| **`SecretStore` is obfuscation** | `deriveMasterKey()` uses a hardcoded `kMasterKeySeed` in the binary. Fine for an API key; **not** for a refresh token. |
| **Don't add vendor-named auth config** | Auth config is generic on purpose: `authMode` is a *protocol* (`apiKey`/`oidc`/`none`), plus `authAuthority`, `authScope`, `authClientId`, `authBackend`, `authHeaderName`/`authHeaderPrefix`. A new identity provider is a new `TokenProvider` **backend**, never new config fields. `normalizeAuthMode()` maps the legacy `entra` value to `oidc`; don't reintroduce it as a stored value. |
| **Don't put a shared key in machine policy** | `HKLM\Software\Policies\JASP` values are readable by every user on the machine. Entra sign-in config is secret-free and fits policy perfectly; an API key does not. |
| **Claude's `inferenceCustomHeaders` is non-secret by policy** | *"Do not put API keys, bearer tokens or other credentials here — this map is stored and distributed as plain configuration."* Match that if you implement a headers map. |
| **clangd diagnostics are useless here** | No Qt include paths are configured, so everything reports `'QObject' file not found`. Verify by reading files, not by trusting diagnostics. |
| **The grep tool only searches the workspace root** | Paths outside it (e.g. `C:\Qt\…`) are silently ignored. Use `terminal` with absolute paths for those. |
| **A previous agent flooded context with the regex `entra\|Entra`** | It matches "**centra**l" in bundled jQuery/plotly. Scope searches with include-patterns or word-exact terms. |
| **Git Bash `tar` fails on `C:\` paths** | Use PowerShell `System.IO.Compression.ZipFile` instead. |
| **Don't `fetch` `fuget.org`** | Currently a squatted domain. |

---

## 10. Environment notes

- **WSL is not installed.** `terminal` commands run in the host shell — **Git Bash** where
  installed, otherwise PowerShell/cmd. Use host path conventions (`C:\...` or `/c/...`, not
  `/mnt/c/...`). Prefer `python` for JSON/parsing work.
- **`git` is available**; project root is `C:\Users\rdoff\work\pro\jasp-desktop`. Run read-only
  git as `git --no-pager …` and prefer `git --no-optional-locks status`.
- **Commit only when explicitly asked, and never push.**
- **Qt is 6.11.2, MSVC 2022**, at `C:\Qt\6.11.2\msvc2022_64`, and the Qt **source** is at
  `C:\Qt\6.11.2\Src`. Verify the NetworkAuth API there
  (`Src/qtnetworkauth/src/oauth/`) rather than trusting memory — this has already paid off.
- **Neither `az` (Azure CLI) nor `pwsh` is installed.** Anything needing Azure CLI means an
  install first, or the portal.
- No third-party binary is fetched at any point.

---

## 11. Still needed from the human

| For | What |
|---|---|
| Step 2 | Confirmation that removing request-body logging is wanted before a pilot |
| Step 3 | UI decisions: where sign-in lives in `PrefsAI.qml`; what to show when signed in |
| Step 4 | Whether `bearerTokenType` should default to `id_token` for gateway presets (recommended) |
| Step 5 | Where the deployment-guide PDF is produced — there is no existing pipeline |
| Step 6 | Merge vs override semantics for policy-pushed AI config |
| Step 7 | Phase 4: OS vault directly, or a small dependency such as QtKeychain |
| Step 8 | A second Entra tenant to verify dynamic consent for shape 3 |

Per-customer questions to gather (plan §9): OS mix, Conditional Access device requirements,
gateway vs direct, which audience their gateway validates, whether it also requires a
subscription key, which claim its quota is keyed on, per-user attribution, data residency/DPA.

**Open Azure question worth raising with Microsoft:** Anthropic ship OS-broker sign-in for
Conditional Access policies requiring a compliant device, justified on data-path grounds (both
ends of the trust relationship stay inside the customer's control). §3b rejected the broker on
*redistribution* grounds. Those are different questions and the second may be answerable.

---

## 12. Reference

- All live Azure values (tenant, client ID, redirect URIs, publisher verification) are in
  **plan §1b**, including the full error chain and how it was resolved.
- Publisher-verification walkthrough, with the gotchas: plan **Appendix B**.
- Windows-broker research record: plan **§3b** and **Appendix D**.
- Deployment shapes, gateway specifics, Claude Desktop parity, managed config: plan **§3c**.
- Diagram conventions in this folder: `.mmd` source + rendered `.svg` +
  `render_mermaid.py`.
