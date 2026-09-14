# Entra ID Authentication for the JASP AI Feature — Implementation Plan

| | |
|---|---|
| **Status** | **Phase 1 complete** — abstraction, config surface and routing in place and verified end to end. Ready for Phase 2. |
| **Scope** | Identity-based auth for the AI chat feature (`AiBridge`) |
| **Driver** | Enterprise customers require identity-based auth; no static API keys distributed to users |
| **Last verified against** | `aiBridge.cpp` L488–557, `aiconfigmodel.h`, `settings.h`, `Desktop/CMakeLists.txt` |

---

## 1. Problem

The AI feature connects to an OpenAI-compatible endpoint using an
`Endpoint` + `API Key` + `Model`. Enterprise customers want their users to
authenticate with **Microsoft Entra ID** (formerly Azure AD) instead of receiving
a shared static API key.

**This must serve multiple customers, not one.** Nothing is hard-coded to a
specific tenant: endpoint, tenant and scope are per-provider configuration, and
the same binary serves all customers. The vendor-side Entra app registration is
**multi-tenant**, so each customer's administrator grants consent once in their
own tenant.

Common requirements:

- No static API keys distributed to end users.
- Auth via the users' existing Entra ID identities.
- Primarily Azure OpenAI; possibly routed through an internal gateway.
- No vendor-hosted proxy (keeps the vendor out of the customer's data path).

Because Azure OpenAI accepts Entra ID **bearer tokens** on the data plane, and
`AiBridge` already sends `Authorization: Bearer <token>`, the HTTP layer is
largely already compatible. The work is **token acquisition and lifecycle**.

---

## 1b. Current state — Azure setup (done)

| Item | Value |
|---|---|
| Entra tenant | `JASPServicesBV.onmicrosoft.com` |
| Verified custom domain | `jasp-services.com` |
| App registration | `JASP AI Desktop` |
| Application (client) ID | `fc57bc92-9de6-405e-8a47-4161cc3e27d3` |
| Platform | Mobile and desktop applications (public client) |
| Supported accounts | Multiple Entra ID tenants (allow all) |
| Redirect URIs | `http://localhost`; `ms-appx-web://Microsoft.AAD.BrokerPlugin/fc57bc92-…`; `msalfc57bc92-…://auth` |
| Delegated permission | Microsoft Cognitive Services → `user_impersonation` |
| Partner Center | Company account, **fully verified** (identity + business) |
| Partner program | Microsoft AI Cloud Partner Program |
| Publisher verification | ✅ Verified — PartnerGlobal ID linked to the app |

**Account structure** (important for future admin work):

- **Company** Partner Center account → signed in with the Entra work account.
  Used for the partner program and publisher verification.
- **Store (Individual)** account → `jasp-stats@outlook.com`. Holds the published
  Store app. Its Entra tenant link was removed when the Company account was made.
- The published Store app is unaffected by any of this.

---

## 2. Design principles

1. **Additive.** Existing providers with an API key keep working, unchanged.
2. **One interface, several backends.** `AiBridge` should not know *how* a
   token is obtained.
3. **No vendor-hosted proxy.** The client's own Azure tenant remains the
   resource; the vendor hosts nothing.
4. **Tokens are short-lived.** Providers own acquisition, caching, refresh, and
   sign-out; `AiBridge` only consumes.
5. **Secrets get the right home.** Long-lived refresh tokens go to the OS vault,
   not `SecretStore`.

---

## 3. Architecture

```
                    ┌──────────────────────────┐
                    │        AiBridge          │
                    │  (HTTP + SSE, tool loop) │
                    └────────────┬─────────────┘
                                 │ ensureToken() / tokenReady
                    ┌────────────▼─────────────┐
                    │  TokenProvider (abstract)│
                    └────────────┬─────────────┘
             ┌───────────────────┼───────────────────┐
             ▼                   ▼                   ▼
   ┌──────────────────┐ ┌───────────────┐ ┌────────────────────┐
   │ ApiKeyToken      │ │ WamToken      │ │ BrowserToken       │
   │ Provider         │ │ Provider      │ │ Provider           │
   │ (today)          │ │ (Windows) ★   │ │ (QtNetworkAuth)    │
   └──────────────────┘ └───────┬───────┘ └─────────┬──────────┘
                                │                   │
                        msalruntime.dll      system browser +
                        → WAM (DPAPI,        loopback PKCE
                          device-bound)      → OS vault for
                                               refresh token
```

### TokenProvider interface (sketch)

```cpp
class TokenProvider : public QObject
{
    Q_OBJECT
public:
    virtual QString authMode() const = 0; // "apiKey" | "oidc" | "none"
    virtual void    ensureToken() = 0;   // async
    virtual QString token() const = 0;   // cached value, may be empty
    virtual bool    isValid() const = 0;
    virtual void    signOut()   = 0;

signals:
    void tokenReady(const QString &token);
    void interactionRequired(const QString &reason);
    void authFailed(const QString &error);
};
```

### Request flow

A request must not block the UI thread on a login dialog. Today `sendToAI()`
reads `authToken()` synchronously (L503). New flow:

1. `AiBridge` calls `ensureToken()`.
2. On `tokenReady` → build and POST the request.
3. On `interactionRequired` → surface a "Sign in" prompt in QML.
4. On HTTP 401 mid-stream → invalidate cache, re-acquire once, retry.

---

## 4. Config model changes

New per-provider fields, persisted inside the existing `AI_USER_PROVIDERS`
JSON blob (no new `Settings::Type` required).

Auth is described by **protocol, never by vendor**, so supporting another
identity provider is configuration rather than new fields. Six axes:

| Axis | Field | Values | Notes |
|---|---|---|---|
| scheme | `authMode` | `apiKey` (default), `oidc`, `none` | named by protocol; `apiKey` preserves today's behavior |
| authority | `authAuthority` | tenant id, `organizations`, or an issuer URL | Entra, Okta and Auth0 all have one |
| scope | `authScope` | e.g. `https://cognitiveservices.azure.com/.default` | the resource the token is requested for |
| app | `authClientId` | optional | empty = the built-in JASP registration; set it when a customer brings their own |
| backend | `authBackend` | `auto` (default), `wam`, `browser`, `devicecode` | *how* a token is acquired, kept separate from *who* issues it |
| wire | `authHeaderName` / `authHeaderPrefix` | `Authorization` / `Bearer ` (defaults) | covers gateways: `api-key`, `X-API-Key`, or a raw `Authorization` (empty prefix) |

Pre-rename keys — `entraTenant`, `entraScope`, and the `entra` scheme value —
are still **read** on load so config written before the rename keeps resolving;
only canonical values are written back.

Add a shipped provider preset to `Resources/defaultProviders.json`:
**"Azure OpenAI (Entra ID)"** — endpoint template, `authMode: "oidc"`, default
scope — so configuration is one click.

---

## 5. Phases

### Phase 0 — Prerequisites — ✅ MOSTLY DONE

- [x] Tenant is Entra-primary (no ADFS involved)
- [x] App registration created (values in §1b)
- [x] Publisher verification complete (PartnerGlobal ID linked)
- [ ] Stand up a free secondary test tenant for multi-tenant consent testing
- [ ] Vendor `msalruntime` header + DLLs (MIT) from the same version

**Remaining:** the two unchecked items above.

**Acceptance:** can complete an admin-consent flow and obtain a token by hand
(Azure CLI or equivalent) against the target resource.

### Phase 1 — Abstraction & config — ✅ DONE

- [x] `Desktop/ai/auth/tokenprovider.h` — interface.
- [x] `Desktop/ai/auth/apitokenprovider.{h,cpp}` — wraps `currentApiKey()`.
- [x] Extend `AIProviderEntry` + `ProviderOverrides` with the §4 fields.
- [x] `AIConfigModel`: `Q_PROPERTY`s, getters/setters, signals; wire into
      `loadUserData()` / `saveUserData()`.
- [x] `Resources/defaultProviders.json`: Azure OpenAI preset.
- [x] `AiBridge`: route through the provider; keep behavior identical for
      `apiKey` mode.
- [x] Generalise the config surface from vendor names (`entraTenant`,
      `entraScope`, scheme `entra`) to the protocol-shaped axes in §4, with a
      back-compat read for already-written config.

**Acceptance:** builds clean; all existing providers behave exactly as today.

✅ Verified: an existing API-key provider (DeepSeek) still works unchanged, and
selecting the Azure OpenAI preset routes through the new config path and reports
*"Sign-in is not available in this build yet."* That exercises shipped JSON →
`AIConfigModel` → `AiBridge::configureTokenProvider()` **before** any OIDC
backend exists, which is the point of this phase.

### Phase 2 — WAM backend (Windows) ★ priority

- [ ] Vendor header + `msalruntime.dll` (x64/arm64); CMake copies DLL to output.
- [ ] `Desktop/ai/auth/wamtokenprovider.{h,cpp}` guarded by `#ifdef Q_OS_WIN`.
      Sequence: `MSALRUNTIME_Startup` → build params → `AcquireTokenSilently`
      → `SigninInteractively` fallback → read token.
- [ ] Pass parent **HWND** from Qt (`winId()`).
- [ ] Map result codes → `tokenReady` / `interactionRequired` / `authFailed`.

**Acceptance:** user signs in on a test machine, gets a token, completes an
Azure OpenAI call — including under a **compliant-device** Conditional Access
policy. This is the proof point for the client.

### Phase 3 — Wiring & UI

- [ ] `sendToAI()` / `testConnection()` become async (token → then POST).
- [ ] Retry once on 401; surface sign-in state to QML.
- [ ] `PrefsAI.qml` Connection group: auth-mode selector; when `oidc`, hide the
      API-key field and show **Sign in / account / Sign out**.

### Phase 4 — Browser backend (cross-platform)

- [ ] Add `Qt::NetworkAuth` to the Qt component list; link `JASPDesktopLib`.
- [ ] `browsertokenprovider.{h,cpp}` using `QOAuth2AuthorizationCodeFlow` +
      `QOAuthHttpServerReplyHandler` (PKCE, ephemeral port).
- [ ] Launch **Edge preferentially** on Windows to maximise device-CA pass rate.
- [ ] `securetokenstore.{h,cpp}` — refresh token via OS vault
      (DPAPI / Keychain / libsecret). Leave `SecretStore` for API keys.

### Phase 5 — Backend selection

- [ ] Auto: Windows → WAM, fall back to browser; mac/Linux → browser.
- [ ] Honour the per-provider `authBackend` override
      (`auto` | `wam` | `browser` | `devicecode`).

### Phase 6 — Hardening & privacy (before pilot)

- [ ] Remove the unconditional request-body log (`aiBridge.cpp` L549–550).
- [ ] Default `m_debugDumpEnabled` to `false`.
- [ ] Sign-out / revocation; shared-machine handling; temp-file cleanup.

### Phase 7 — Test matrix

- [ ] Test tenant + admin consent.
- [ ] Silent token, refresh, 401 mid-stream retry.
- [ ] CA compliant-device policy (pass + fail paths).
- [ ] Non-Windows fallback.
- [ ] Add slots per `Tests/` conventions.

---

## 6. File inventory

**New**

| Path | Purpose |
|---|---|
| `Desktop/ai/auth/tokenprovider.h` | Interface — ✅ Phase 1 |
| `Desktop/ai/auth/apitokenprovider.{h,cpp}` | Existing key behavior — ✅ Phase 1 |
| `Desktop/ai/auth/wamtokenprovider.{h,cpp}` | WAM (Windows) — Phase 2 |
| `Desktop/ai/auth/browsertokenprovider.{h,cpp}` | Loopback PKCE — Phase 4 |
| `Desktop/utilities/securetokenstore.{h,cpp}` | OS-vault refresh-token storage — Phase 4 |

**Modified**

| Path | Change |
|---|---|
| `Desktop/ai/aiBridge.{h,cpp}` | Async token resolution; provider routing |
| `Desktop/gui/aiconfigmodel.{h,cpp}` | New auth fields + persistence |
| `Desktop/components/JASP/Widgets/FileMenu/PrefsAI.qml` | Auth-mode UI |
| `Resources/defaultProviders.json` | Azure OpenAI preset |
| `Desktop/CMakeLists.txt` | `Qt::NetworkAuth`; msalruntime DLL copy |

> Note: `JASPDesktopLib` sources are collected with `file(GLOB_RECURSE ...)`, so
> CMake must be re-run after adding files.

---

## 7. Risks

| Risk | Impact | Mitigation |
|---|---|---|
| No official MSAL C++ SDK | Medium | Use the `msalruntime` C API (same layer MSAL Python/Java use) |
| `msalruntime.dll` packaging | Medium | Post-build copy; test **MSIX/Store** package specifically |
| Client uses ADFS | High | Confirm Phase 0; browser fallback if so |
| `SecretStore` master key is a binary constant | Medium | Never store refresh tokens there; use OS vault |
| Conditioning on external tenant setup | High | Phase 0 before Phase 2 validation |
| Windows-only WAM | Low | Browser backend covers mac/Linux |

---

## 8. Code-health items found

These should be addressed before a client pilot:

1. **Unconditional prompt logging.** `AiBridge::sendToAI()` logs the full
   request body to the application log on every request (L549–550), not only in
   debug mode. In an enterprise this writes research data to log files.
2. **Debug dump default on.** `m_debugDumpEnabled = true` writes request bodies
   to `<tempDir>/ai-request.json`.
3. **`SecretStore` is obfuscation, not confidentiality.** `deriveMasterKey()`
   uses a hardcoded `kMasterKeySeed` compiled into the binary. Acceptable for an
   API key; not acceptable for an Entra refresh token.

---

## 9. Per-customer onboarding questions

This ships to multiple customers, so gather these **before each deployment**.
None of them block development.

1. What **OS mix** do their users run? (Decides how much WAM matters.)
2. Do their Conditional Access policies require a **compliant / hybrid-joined
   device**? (Decides WAM vs browser reliability.)
3. Do they route through a **gateway** (APIM / LiteLLM)? Which auth header?
4. Is **per-user usage / chargeback** a requirement? (Azure OpenAI does not
   record caller identity — requires a gateway.)
5. **Data residency**, no-training confirmation, DPA.

---

## Appendix A — Entra app registration checklist

| Setting | Value |
|---|---|
| Supported accounts (`signInAudience`) | `AzureADMultipleOrgs` (work/school only) |
| Platform | **Mobile and desktop applications** (not Web, not SPA) |
| `allowPublicClient` | `true` |
| Redirect URI (browser) | `http://localhost` — **port is ignored when matching** |
| Redirect URI (WAM) | `ms-appx-web://Microsoft.AAD.BrokerPlugin/{client_id}` |
| Delegated permission | Microsoft Cognitive Services → `user_impersonation` |
| Resource-side RBAC (client tenant) | `Cognitive Services OpenAI User` |
| Admin consent URL | `https://login.microsoftonline.com/{tenant}/adminconsent?client_id={client_id}` |

Notes:

- IPv6 loopback `[::1]` is not supported.
- `http://127.0.0.1` (more firewall-robust) must be added via the manifest
  (`replyUrlsWithType`); the portal text box rejects it.
- Prefer `Cognitive Services OpenAI User` over `Cognitive Services User` — the
  latter includes `listkeys`, which defeats the purpose of removing keys.

## Appendix B — Publisher verification (how we actually did it)

Recorded because it was the hardest part of the setup, and the same path
applies to any future Microsoft app we publish.

1. Entra tenant created; `jasp-services.com` verified via DNS TXT.
2. Partner Center: **Company** account (Individual cannot be used for this).
   - Individual accounts must use a personal MSA, and cannot be converted.
   - A Company account can sign in with an Entra work account; onboarding it
     onboards the whole tenant.
3. Join **Microsoft AI Cloud Partner Program** (Account settings → Programs).
4. Set the **Primary Contact** to a working `@jasp-services.com` mailbox — this
   is the domain-match requirement.
5. Upload proof of domain ownership. Microsoft accepts exactly three types:
   - WHOIS record
   - Domain registration / renewal invoice
   - Assignment letter from an authorised representative

   Documents must be under 12 months old (or expire more than 2 months out),
   and the entity name + domain must match the profile exactly.
6. Get the **PartnerGlobal (PGA)** ID — *not* the Partner Location ID.
7. Entra app → **Branding & properties** → publisher domain = `jasp-services.com`.
8. Entra app → **Add MPN ID to verify publisher** → paste the PartnerGlobal ID.

**Gotchas hit along the way:** `*.onmicrosoft.com` can never be the publisher
domain; the Partner Location ID is rejected; an Individual account cannot be
upgraded to Company; a tenant can only be linked to one Partner Center account.

## Appendix C — References

- Microsoft identity platform: authorization code flow + PKCE
- Redirect URI (reply URL) best practices and limitations
- Using MSAL with Web Account Manager (WAM) — MSAL.NET / Python / Java
- Configure Azure OpenAI with Microsoft Entra ID authentication
- Conditional Access: device-based conditions and browser support
- Partner Center: verification responses; publisher verification overview
- LiteLLM: OIDC/JWT auth (enterprise feature), pricing
