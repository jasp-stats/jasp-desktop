# Entra ID Authentication for the JASP AI Feature — Implementation Plan

| | |
|---|---|
| **Status** | Phase 1 complete and verified. Browser sign-in backend **written, not yet built or run**. |
| **Scope** | Identity-based auth for the AI chat feature (`AiBridge`) |
| **Driver** | Enterprise customers require identity-based auth; no static API keys distributed to users |
| **Decision** | **System browser + loopback PKCE** (QtNetworkAuth). **No third-party binaries.** |
| **Last verified against** | `Desktop/ai/aiBridge.{h,cpp}`, `Desktop/ai/auth/`, `Desktop/gui/aiconfigmodel.{h,cpp}`, Qt 6.11.2 |

---

## 1. Problem

The AI feature connects to an OpenAI-compatible endpoint using an
`Endpoint` + `API Key` + `Model`. Enterprise customers want their users to
authenticate with **Microsoft Entra ID** (formerly Azure AD) instead of receiving
a shared static API key.

**This must serve multiple customers, not one.** Nothing is hard-coded to a
specific tenant: endpoint, authority and scope are per-provider configuration, and
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
already compatible. The work is **token acquisition and lifecycle**.

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

The `ms-appx-web://…BrokerPlugin…` redirect URI is left over from the original
broker plan (§3b) and is unused by the browser flow. It can stay without harm.

---

## 2. Design principles

1. **Additive.** Existing providers with an API key keep working, unchanged.
2. **One interface, several backends.** `AiBridge` must not know *how* a token is
   obtained.
3. **No vendor-hosted proxy.** The client's own Azure tenant remains the
   resource; the vendor hosts nothing.
4. **Tokens are short-lived.** Backends own acquisition, caching, refresh, and
   sign-out; `AiBridge` only consumes.
5. **No third-party binaries, and no vendored code we cannot audit.** Everything
   shipped must be free software with published source and a clear licence —
   which is what rules out the Windows broker (§3b).
6. **Never an embedded web view for sign-in.** Conditional Access
   device-compliance fails inside embedded views. System browser only.

---

## 3. Architecture

```
                    ┌──────────────────────────┐
                    │        AiBridge          │
                    │  (HTTP + SSE, tool loop) │
                    └────────────┬─────────────┘
                                 │ isValid() / token() / ensureToken()
                    ┌────────────▼─────────────┐
                    │  TokenProvider (abstract)│
                    └────────────┬─────────────┘
                                 │
              ┌──────────────────┴───────────────────┐
              ▼                                      ▼
   ┌──────────────────────┐            ┌──────────────────────────┐
   │ ApiKeyTokenProvider  │            │ BrowserTokenProvider     │
   │ static key (default) │            │ system browser + loopback│
   │ synchronous, no UI   │            │ PKCE via QtNetworkAuth   │
   └──────────────────────┘            └──────────────────────────┘
```

### TokenProvider interface

```cpp
class TokenProvider : public QObject
{
    Q_OBJECT
public:
    virtual QString   authMode() const = 0;   // "apiKey" | "oidc" | "none"
    virtual void      ensureToken() = 0;      // async; never blocks
    virtual QString   token() const = 0;      // cached value, may be empty
    virtual bool      isValid() const = 0;    // usable right now (safety margin applied)
    virtual QDateTime expiresAt() const = 0;  // for UI/diagnostics
    virtual QString   accountName() const = 0;
    virtual void      signOut() = 0;          // local only

signals:
    void tokenReady(const QString &token);
    void interactionRequired(const QString &message);   // display-ready
    void authFailed(const QString &error);              // display-ready
};
```

### Request flow — and why `AiBridge` defers rather than blocks

A browser sign-in needs the event loop to keep running, so the request path
cannot sit and wait for a token. `sendToAI()` / `testConnection()` therefore:

1. `configureTokenProvider()`.
2. If the provider `isValid()` → proceed exactly as before, synchronously. This
   is the whole `apiKey` path, and it is unchanged.
3. Otherwise park the request (`m_pendingSend` / `m_pendingTest`) and call
   `ensureToken()`, then return.
4. `onTokenReady()` resumes the parked request. `onAuthFailed()` unwinds it and
   reports. `onAuthInteractionRequired()` forwards a message for the UI while the
   request stays parked.

If the user hits stop while signing in, `stopStream()` clears `m_streaming` and
the parked request is dropped rather than revived.

---

## 3b. Why the Windows broker (WAM / `msalruntime`) is not used

This was the original Phase 2, and it is **rejected on auditability and licence
grounds**. Recorded here so nobody re-derives it, and because "just use WAM" is
the obvious suggestion.

**What it is.** Microsoft has no public, general-purpose C++ MSAL. The broker is
reached through `msalruntime`, published as the NuGet package
`Microsoft.Identity.Client.NativeInterop`. It is a thin wrapper over **OneAuth**,
Microsoft's internal authentication stack shared by Office, Teams, Visual Studio
and Windows. The SDKs above it (MSAL.NET / Python / Node / Java) are open source;
the runtime underneath has never been published.

**The findings, all verified from the artifacts themselves:**

| Finding | Evidence |
|---|---|
| No source, no header, no import library | The `.nupkg` contains only DLLs, .NET interop assemblies, MSBuild targets and a `LICENSE`. The PyPI wheel contains only the DLL, a compiled `.pyd` and stubs. |
| Source repo is internal | nuspec `projectUrl`/`repository` = `office.visualstudio.com/_git/OneAuth`. The public C++ repo older versions pointed at (`AzureAD/microsoft-authentication-library-for-cpp`) now **404s**, and GitHub repo search returns **zero** results. |
| Contradictory licence terms, same version | 0.20.6 declares **Microsoft proprietary Software License Terms** on NuGet (and ships that text), while the npm package `@azure/msal-node-runtime` 0.20.6 declares `"license": "MIT"`, and the PyPI wheel's `METADATA` says MIT while bundling the **same EULA text**. |
| The change is recent | 0.20.2, 0.20.3 and 0.20.4 all declare **MIT** by SPDX expression with no `LICENSE` file at all; only **0.20.6** switched to the EULA. |
| The EULA forbids distribution | §1(a) permits install/use; **§3(e)** prohibits to "share, publish, distribute, or lease the software… or transfer… to any third party". §2(a) also carries a telemetry notice. |

**Reading:** because the runtime is a closed binary from an internal repo, the
most coherent explanation is that the runtime was never Microsoft's to license
under MIT, and **0.20.6 is the correction** — which would mean the MIT markers on
some packages are the error, not the EULA. That is inference, not fact, but it is
the reading that fits all the evidence. Only Microsoft can confirm.

**Why it matters here specifically.** `Docs/development/jasp-licensing.md` records
that the JASP GUI binary is released under **AGPL3+**. Shipping a proprietary,
non-auditable binary inside it is a problem twice over: a right-to-distribute
question, and an AGPL-compatibility question. Note the contrast with the Microsoft
binaries JASP already ships (VC++ redistributables) — those come with an explicit
redistributable grant; this does not.

**What was ruled out along the way:**

- `azure-identity-cpp` (vcpkg) — **not applicable.** Its credentials are
  `AzureCli`, `AzurePipelines`, `Chained`, `ClientAssertion`, `ClientCertificate`,
  `ClientSecret`, `DefaultAzure`, `Environment`, `ManagedIdentity`,
  `WorkloadIdentity`. There is **no `InteractiveBrowserCredential` and no
  `DeviceCodeCredential`** — it is built for daemons, services and managed
  identities, not for a person signing in at a desk.
- MSAL.NET hosted from C++ — would solve the support question but **not the
  licence question**: it pulls in the same `NativeClientInterop` package and ships
  the same binary, at the cost of a .NET runtime dependency on three platforms.
- `WebAuthenticationCoreManager` (WinRT) — the one genuine no-binary route to
  WAM, and still worth a spike **if** a customer's CA policy ever makes brokered
  auth mandatory. It is UWP-oriented, undocumented for unpackaged Win32 apps, and
  Windows-only. Not a v1 dependency.

**Consequence:** if a prospect mandates brokered auth, JASP cannot serve them
until this is resolved with Microsoft. That is a business risk, accepted
deliberately rather than paid for with an unshippable binary. §9 already gathers
the Conditional Access question per customer; that is where this surfaces.

---

## 4. Config model changes

Per-provider fields, persisted inside the existing `AI_USER_PROVIDERS` JSON blob
(no new `Settings::Type`).

Auth is described by **protocol, never by vendor**, so supporting another
identity provider is configuration rather than new fields. Six axes:

| Axis | Field | Values | Notes |
|---|---|---|---|
| scheme | `authMode` | `apiKey` (default), `oidc`, `none` | named by protocol; `apiKey` preserves today's behavior |
| authority | `authAuthority` | tenant id, `organizations`, or an issuer URL | Entra, Okta and Auth0 all have one |
| scope | `authScope` | e.g. `https://cognitiveservices.azure.com/.default` | the resource the token is requested for |
| app | `authClientId` | optional | empty = the built-in JASP registration |
| backend | `authBackend` | `auto` (default), `browser`, `devicecode` | *how* a token is acquired |
| wire | `authHeaderName` / `authHeaderPrefix` | `Authorization` / `Bearer ` (defaults) | covers gateways |

Pre-rename keys — `entraTenant`, `entraScope`, and the `entra` scheme value — are
still **read** on load so config written before the rename keeps resolving; only
canonical values are written back.

Shipped preset in `Resources/defaultProviders.json`: **"Azure OpenAI (Entra ID)"**
— endpoint template, `authMode: "oidc"`, default scope.

### How the config maps onto the request

`BrowserTokenProvider` turns the generic fields into OIDC endpoints:

- `authAuthority` → authority base (`{tenant}` and `organizations` are prefixed
  with `https://login.microsoftonline.com/`; a full URL is used as-is; a trailing
  `/v2.0` is trimmed), then
  `…/oauth2/v2.0/authorize` and `…/oauth2/v2.0/token`.
- `authScope` → requested scope, **plus `openid profile offline_access`**. The
  first three are what make the account name and silent refresh possible; without
  `offline_access` Entra returns no refresh token and every expiry means a fresh
  sign-in.
- `authClientId` → `setClientIdentifier()`, defaulting to the JASP registration.
  Public client: **no client secret** — PKCE is what protects the code.

---

## 5. Phases

### Phase 0 — Prerequisites — ✅ DONE

- [x] Tenant is Entra-primary (no ADFS involved)
- [x] App registration created (values in §1b)
- [x] Publisher verification complete (PartnerGlobal ID linked)
- [x] Redirect URI `http://localhost` registered
- [ ] *(dropped)* Vendor `msalruntime` header + DLLs — superseded by §3b
- [ ] Stand up a free secondary test tenant for multi-tenant consent testing

### Phase 1 — Abstraction & config — ✅ DONE

- [x] `Desktop/ai/auth/tokenprovider.h` — interface.
- [x] `Desktop/ai/auth/apitokenprovider.{h,cpp}` — wraps `currentApiKey()`.
- [x] Extend `AIProviderEntry` + `ProviderOverrides` with the §4 fields.
- [x] `AIConfigModel`: `Q_PROPERTY`s, getters/setters, signals; wired into
      `loadUserData()` / `saveUserData()`.
- [x] `Resources/defaultProviders.json`: Azure OpenAI preset.
- [x] `AiBridge`: routes through a provider; `apiKey` unchanged.
- [x] Generalise the config surface from vendor names to the §4 axes, with a
      back-compat read.

**Acceptance met:** an existing API-key provider still works unchanged, and the
Azure preset routes through the new path (reporting that sign-in was not
available in that build, before any backend existed).

### Phase 2 — Browser backend — 🚧 CODE WRITTEN, NOT YET BUILT OR RUN

- [x] `Desktop/ai/auth/browsertokenprovider.{h,cpp}` —
      `QOAuth2AuthorizationCodeFlow` + `QOAuthHttpServerReplyHandler`, PKCE S256,
      system browser, `autoRefresh` + 5-minute `refreshLeadTime`.
- [x] `Qt::NetworkAuth` added to the Qt components and `JASPDesktopLib` link list.
- [x] `AiBridge`: provider selection by `authMode`/`authBackend`; the deferred
      request path; `signIn()` / `signOut()` / `isSignedIn()`;
      `authStateChanged()` / `authInteractionRequired()`.
- [ ] **Build and run it.** Nothing below is validated until this happens.
- [ ] Re-check the redirect URI Qt actually produces — logged on every attempt as
      `BrowserTokenProvider: sign-in redirect URI is …`. `setCallbackHost` is
      required: Qt's default advertises `127.0.0.1`, but the registration only
      accepts `localhost` (Appendix A), which would be `AADSTS50011`.
- [ ] Retry once on HTTP 401. Deliberately **not** done yet: it touches the
      streaming teardown in `onReplyFinished()`, which is the riskiest code in the
      file, and the provider already renews ahead of expiry.

**Acceptance:** a user signs in via the browser and completes an Azure OpenAI
call; an existing API-key provider still behaves exactly as today.

### Phase 3 — UI

- [ ] `PrefsAI.qml`: sign-in method selector; hide the API-key field when
      `authMode` is `oidc`; **Sign in / account / Sign out**; surface
      `authInteractionRequired` and auth errors.
- [ ] Show the signed-in account (`accountName()`) and expiry (`expiresAt()`).

### Phase 4 — Token persistence (OS vault)

- [ ] `securetokenstore.{h,cpp}` — refresh token via DPAPI / Keychain / libsecret.
      `SecretStore` is **not** suitable: `deriveMasterKey()` uses a hardcoded
      `kMasterKeySeed`, which is obfuscation, not confidentiality.
- [ ] Until this exists, the user signs in once per JASP run.

### Phase 5 — Device-code fallback

- [ ] `DeviceCodeTokenProvider` on `QOAuth2DeviceAuthorizationFlow`. Pure HTTPS:
      no browser, no loopback listener, no binaries. Covers locked-down machines,
      broken default browsers, remote/kiosk sessions.
- [ ] `authBackend: "devicecode"` currently reports "not implemented yet" — wire
      it up here.
- [ ] Caveats to document: clunky UX, weaker against device code phishing, and it
      cannot satisfy device-compliance Conditional Access (no device presented).

### Phase 6 — Hardening & privacy (before pilot)

- [ ] Remove the **unconditional request-body log** (`aiBridge.cpp`, in
      `postStreamingRequest`). It writes full request bodies on every request.
- [ ] Default `m_debugDumpEnabled` to `false` (currently `true`).
- [ ] Sign-out / revocation; shared-machine handling; temp-file cleanup.

### Phase 7 — Test matrix

- [ ] Multi-tenant consent; silent refresh; expiry mid-session.
- [ ] CA compliant-device policy (pass + fail paths).
- [ ] Non-Windows (macOS / Linux) sign-in.
- [ ] Add slots per `Tests/` conventions.

### Deferred — broker (conditional, licence-gated)

Only if a customer's CA policy makes brokered auth mandatory. Entry criteria:
(a) a customer states the requirement, and (b) the licence question in §3b is
resolved in writing. First thing to try is `WebAuthenticationCoreManager`, since
it needs no shipped binary.

---

## 6. File inventory

**New**

| Path | Purpose | State |
|---|---|---|
| `Desktop/ai/auth/tokenprovider.h` | Interface | ✅ Phase 1 |
| `Desktop/ai/auth/apitokenprovider.{h,cpp}` | Existing key behavior | ✅ Phase 1 |
| `Desktop/ai/auth/browsertokenprovider.{h,cpp}` | System browser + loopback PKCE | 🚧 Phase 2, unbuilt |
| `Desktop/ai/auth/devicecodetokenprovider.{h,cpp}` | Device-code fallback | Phase 5 |
| `Desktop/utilities/securetokenstore.{h,cpp}` | OS-vault refresh-token storage | Phase 4 |

**Modified**

| Path | Change |
|---|---|
| `Desktop/ai/aiBridge.{h,cpp}` | Provider selection, deferred request path, sign-in API |
| `Desktop/gui/aiconfigmodel.{h,cpp}` | Auth fields + persistence |
| `Tools/CMake/Libraries.cmake`, `Desktop/CMakeLists.txt` | `NetworkAuth` component + link |
| `Desktop/components/JASP/Widgets/FileMenu/PrefsAI.qml` | Auth UI (Phase 3) |
| `Resources/defaultProviders.json` | Azure OpenAI preset |

> `JASPDesktopLib` collects sources with `file(GLOB_RECURSE …)`, so CMake must be
> **re-run** after adding the new auth files.

---

## 7. Risks

| Risk | Impact | Mitigation |
|---|---|---|
| No supported C++ SDK from Microsoft | Medium | Browser flow uses standard OAuth 2.0 + OIDC; nothing Microsoft-specific to break |
| CA policy that *mandates* brokered auth | High | Cannot be served today — see §3b. Surfaced by the §9 onboarding questions, not discovered at pilot |
| Loopback blocked / browser cannot launch | Low | Device-code fallback (Phase 5) |
| No token persistence yet | Medium | Server-side SSO cookie means re-sign-in is usually silent; Phase 4 removes it |
| ADFS / B2C tenants | Medium | Broker unsupported there anyway; the browser flow is the documented fallback |
| `SecretStore` master key is a binary constant | Medium | Never store refresh tokens there — Phase 4 uses the OS vault |
| Windows-only broker | — | No longer relevant: the browser flow is cross-platform |

---

## 8. Code-health items found

Address before a client pilot:

1. **Unconditional prompt logging.** `AiBridge::postStreamingRequest()` logs the
   full request body on every request, not only in debug mode. In an enterprise
   this writes research data to log files.
2. **Debug dump default on.** `m_debugDumpEnabled = true` writes request bodies to
   `<tempDir>/ai-request.json`.
3. **`SecretStore` is obfuscation, not confidentiality.** See §7.

---

## 9. Per-customer onboarding questions

Gather these **before each deployment**. None block development.

1. What **OS mix** do their users run?
2. Do their Conditional Access policies require a **compliant / hybrid-joined
   device**, or **brokered authentication specifically**? (The second is the one
   we currently cannot serve — §3b.)
3. Do they route through a **gateway** (APIM / LiteLLM)? Which auth header?
4. Is **per-user usage / chargeback** a requirement? (Azure OpenAI does not record
   caller identity — requires a gateway.)
5. **Data residency**, no-training confirmation, DPA.

---

## Appendix A — Entra app registration checklist

| Setting | Value |
|---|---|
| Supported accounts (`signInAudience`) | `AzureADMultipleOrgs` (work/school only) |
| Platform | **Mobile and desktop applications** (not Web, not SPA) |
| `allowPublicClient` | `true` |
| Redirect URI (browser) | `http://localhost` — **port is ignored when matching** |
| Delegated permission | Microsoft Cognitive Services → `user_impersonation` |
| Resource-side RBAC (client tenant) | `Cognitive Services OpenAI User` |
| Admin consent URL | `https://login.microsoftonline.com/{tenant}/adminconsent?client_id={client_id}` |

Notes:

- The loopback redirect must match **`localhost` by name**. `127.0.0.1` is not
  equivalent for the matcher and the portal text box rejects it; it must be added
  via the manifest (`replyUrlsWithType`). This is why
  `BrowserTokenProvider` calls `setCallbackHost("localhost")` explicitly.
- IPv6 loopback `[::1]` is not supported (Qt tries IPv4 first, so this is fine in
  practice).
- Prefer `Cognitive Services OpenAI User` over `Cognitive Services User` — the
  latter includes `listkeys`, which defeats the purpose of removing keys.

## Appendix B — Publisher verification (how we actually did it)

Recorded because it was the hardest part of the setup, and the same path applies
to any future Microsoft app we publish.

1. Entra tenant created; `jasp-services.com` verified via DNS TXT.
2. Partner Center: **Company** account (Individual cannot be used for this).
   - Individual accounts must use a personal MSA, and cannot be converted.
   - A Company account can sign in with an Entra work account; onboarding it
     onboards the whole tenant.
3. Join **Microsoft AI Cloud Partner Program** (Account settings → Programs).
4. Set the **Primary Contact** to a working `@jasp-services.com` mailbox — this is
   the domain-match requirement.
5. Upload proof of domain ownership. Microsoft accepts exactly three types:
   - WHOIS record
   - Domain registration / renewal invoice
   - Assignment letter from an authorised representative

   Documents must be under 12 months old (or expire more than 2 months out), and
   the entity name + domain must match the profile exactly.
6. Get the **PartnerGlobal (PGA)** ID — *not* the Partner Location ID.
7. Entra app → **Branding & properties** → publisher domain = `jasp-services.com`.
8. Entra app → **Add MPN ID to verify publisher** → paste the PartnerGlobal ID.

**Gotchas hit along the way:** `*.onmicrosoft.com` can never be the publisher
domain; the Partner Location ID is rejected; an Individual account cannot be
upgraded to Company; a tenant can only be linked to one Partner Center account.

## Appendix C — References

- Microsoft identity platform: authorization code flow + PKCE
- Redirect URI (reply URL) best practices and limitations
- Configure Azure OpenAI with Microsoft Entra ID authentication
- Conditional Access: device-based conditions and browser support
- Qt Network Authorization: `QOAuth2AuthorizationCodeFlow`,
  `QOAuthHttpServerReplyHandler`, OAuth 2.0 overview
- Partner Center: verification responses; publisher verification overview
- LiteLLM: OIDC/JWT auth (enterprise feature), pricing

## Appendix D — Windows broker (msalruntime) research record

Kept because the findings are non-obvious and took real work, and the question
will come up again.

- **Package:** `Microsoft.Identity.Client.NativeInterop`, latest **0.20.6**, on
  nuget.org. Ships `runtimes/win-{x64,arm64,x86}/native/msalruntime*.dll`, plus
  macOS dylibs and a Linux `.so`. No header, no `.lib`.
- **Licence timeline:** ≤ 0.20.4 = `MIT` (SPDX expression, no `LICENSE` file in
  the package, no EULA text anywhere). **0.20.6** = `license type="file"` pointing
  at Microsoft's proprietary EULA.
- **Cross-channel contradiction:** npm `@azure/msal-node-runtime@0.20.6` declares
  `"license": "MIT"`; PyPI `pymsalruntime@0.20.6` declares MIT in `METADATA` while
  bundling the EULA text as its `LICENSE` file.
- **Not the same binary across channels:** the NuGet 0.20.6 and PyPI 0.20.6 DLLs
  differ in size and SHA-256, as do 0.20.4 vs 0.20.6.
- **The broker itself is an OS component**, not something apps ship: WAM on
  Windows, `microsoft-identity-broker` (apt/dnf) on Linux, the Enterprise SSO
  plug-in on macOS. `msalruntime` is only the client-side glue — which is why
  Microsoft ships it for Linux. That does **not** grant redistribution rights to
  anyone else; a licensor distributing its own software says nothing about
  licensees.
- **Supported broker paths are SDK-only:** MSAL.NET / Python / Node / Java. The
  npm package states it is "not intended or supported for direct use". There is no
  supported C++ path at all, which is why nothing here is documented for us.
- **Known packaging landmine** (if ever revisited): MSAL.NET issue #3740 — the
  `.NET` loader expects `runtimes\win-<arch>\native`, which breaks under MSIX.
