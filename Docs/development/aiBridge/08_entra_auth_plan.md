# Entra ID Authentication for the JASP AI Feature — Implementation Plan

| | |
|---|---|
| **Status** | Phase 1 complete and verified. Browser sign-in backend **written, not yet built or run**. |
| **Scope** | Identity-based auth for the AI chat feature (`AiBridge`) |
| **Driver** | Enterprise customers require identity-based auth; no static API keys distributed to users |
| **Decision** | **System browser + loopback PKCE** (QtNetworkAuth). **No third-party binaries.** |
| **Last verified against** | `Desktop/ai/aiBridge.{h,cpp}`, `Desktop/auth/`, `Desktop/gui/aiconfigmodel.{h,cpp}`, Qt 6.11.2 — macOS arm64 end-to-end 2026-09-23 |

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

## 1b. Current state — Azure setup (done; sign-in verified)

| Item | Value |
|---|---|
| Entra tenant | `JASPServicesBV.onmicrosoft.com` |
| Verified custom domain | `jasp-services.com` |
| App registration | `JASP AI Desktop` |
| Application (client) ID | `fc57bc92-9de6-405e-8a47-4161cc3e27d3` |
| Platform | Mobile and desktop applications (public client) |
| Supported accounts | Multiple Entra ID tenants (allow all) |
| Redirect URIs | `http://localhost`; `ms-appx-web://Microsoft.AAD.BrokerPlugin/fc57bc92-…`; `msalfc57bc92-…://auth` |
| Delegated permission | Microsoft Cognitive Services → `user_impersonation` — **NOT YET GRANTED**, see below |
| Partner Center | Company account, **fully verified** (identity + business) |
| Partner program | Microsoft AI Cloud Partner Program |
| Publisher verification | ✅ Verified — PartnerGlobal ID linked to the app |

**RESOLVED — sign-in verified end-to-end.** Kept in full because the sequence is the
map for onboarding any customer tenant, and because three of the four failures were
misleading. The chain, in order:

1. ~~`AADSTS650057` — permission missing from `requiredResourceAccess`~~ — **fixed**
   by adding the delegated permission through the manifest (Appendix A):

   ```
   AADSTS650057: Invalid resource … Resource app ID: 7d312290-….
   List of valid resources from app registration: 00000003-0000-0000-c000-000000000000
   ```

   `00000003-…` is Microsoft Graph — the app previously held no Cognitive Services
   permission at all.

2. **`AADSTS650052` — the signing-in tenant has no service principal for the
   *resource* app** `7d312290-…`:

   ```
   The app is trying to access a service '7d312290-…'(Microsoft Cognitive Services)
   that your organization '<tenant-id>' lacks a service principal for.
   ```

   Entra names the remedy in the same message: *"…or consent to the application in
   order to create the required service principal."*

3. **`AADSTS700016` on the admin-consent URL** — it was aimed at tenant
   `1ef0aea6-…` and found no app there. This was a **wrong-tenant redirect on our
   side**: that GUID was lifted from the *sign-in* error, not from the app
   registration. Consent from inside the app's own directory in the portal avoids the
   guessing entirely.

4. **The real blocker.**

   ```
   Could not grant admin consent. Your organization does not have a subscription
   (or service principal) for the following API(s): Microsoft Graph,
   Microsoft Cognitive Services
   ```

   Consent cannot be granted at all, because **the tenant has no Azure subscription
   associated with it.** Service principals for first-party Azure APIs are provisioned
   according to the products a tenant owns; a bare Entra tenant, created only to hold
   an app registration, has none. That `Microsoft Graph` appears in the list is the
   tell — even Graph's SP is absent, which is impossible in a tenant with any product
   attached.

   **This is a test-environment problem, not a product problem.** Any customer running
   Azure OpenAI necessarily has an Azure subscription in their tenant, so the service
   principals exist and consent succeeds. The fence exists only because this tenant
   has never owned an Azure product.

   Resolutions, best first:

   - **Attach an Azure subscription to the tenant.** Unblocks consent *and* supplies
     the resource needed for the end-to-end test. An Azure free account suffices; a
     payment method is required to create the subscription.
   - **Create the SP directly** — a Graph write rather than a portal consent:
     `az ad sp create --id 7d312290-28c8-473c-a0ed-8e53749b6d6d`, signed in with
     `az login --allow-no-subscriptions`. Whether Entra permits this in a
     product-less tenant is **unverified**.
   - **Consent from a tenant that already owns an M365 or Azure product.** Do **not**
     register a second app there — keep this multi-tenant registration as the single
     source of truth and consent to it from the other tenant.

**Two independent gates.** Keep these separate:

| Gate | Question | Decided by | State |
|---|---|---|---|
| 1. **Issuance** | may this app obtain a token for this audience? | Entra consent — which first requires the tenant to own the API's service principal | **blocked: test tenant owns no subscription** |
| 2. **Authorization** | may this user do this thing? | Azure RBAC on the resource | not configured (no resource) |

Gate 1 alone yields a perfectly *valid* token that opens nothing. Gate 2 additionally
needs a resource, a deployment and a role assignment. Consent is therefore not a
privilege escalation: it grants the ability to *ask*, and the resource separately
decides *whether*.

Gate 1 also has a precondition that earlier drafts of this document got wrong:
**consent presupposes the tenant owns the API.** "Sign-in needs no Azure resource"
holds for a tenant that already has a product attached, and fails for an empty one.

### How it was actually resolved

Creating Azure OpenAI resource `test888` in a tenant with a real subscription did two
things at once:

- it provisioned the Cognitive Services service principal, clearing `650052`; and
- it made `user_impersonation` consentable — and consent arrived as **user consent**,
  not admin consent.

A valid token then still returned
`401 PermissionDenied: Principal does not have access to API/Operation`. The last
missing piece was **data-plane RBAC**: `Cognitive Services OpenAI User` on the
resource. Owner (control plane) grants nothing here — see §3c.

**Two lessons worth keeping:**

1. **Admin consent was never required.** For one person signing in, that person's own
   consent is sufficient. Admin consent is purely a convenience for *other* users.
   Earlier drafts insisted otherwise and sent us a long way down the wrong road.
2. **The signed-in account must belong to the tenant that owns the resource.** The
   preset authority is `organizations`, so *any* work account is accepted and the
   token is minted for that account's tenant. A token for the wrong identity produces
   exactly the same `PermissionDenied` as a missing role — indistinguishable without
   the token's `oid`. `BrowserTokenProvider` now logs `oid` and `tid` for that reason.

**The token's `aud` decides who will accept it.** A token minted for
`https://cognitiveservices.azure.com` is accepted by Azure OpenAI / Azure AI Foundry
endpoints directly, because that is the audience they expect.

A gateway in front (APIM, LiteLLM) is a **separate** decision, made by the customer's
gateway policy, not by us:

- If the gateway validates an incoming token with that same audience (or accepts and
  forwards it), our token works unchanged.
- If the gateway publishes its own app registration and scope (e.g.
  `api://their-gateway/access_as_user`) and demands tokens for *that* audience, then
  our token is rejected by design and the configured scope has to change to theirs.
- Many gateways also expect the credential in a non-standard header, which is why
  `authHeaderName` / `authHeaderPrefix` exist as configuration.

**Preset specifics that matter for testing** (`Resources/defaultProviders.json`,
provider `f2b9a4d1-…`):

- `authAuthority` is **`organizations`**, not a fixed tenant. Sign-in therefore lands
  in whichever tenant owns the account being used, and **consent must be granted in
  that tenant**. This is exactly what produced the `1ef0aea6-…` mix-up: Entra named
  the *target* tenant, while the app registration and its permissions live in the
  vendor tenant. When testing, deliberately sign in with an account from the tenant
  you intend to consent in. `organizations` is right for shipping — it is what lets
  one binary serve every customer — it is merely easy to misread while testing.
- The endpoint is the **v1** surface:
  `https://<resource>.openai.azure.com/openai/v1/chat/completions`, with empty
  `extraParams` — the v1 API needs **no `api-version`** query parameter. So the
  resource value to change is the hostname only.
- The `model` field for Azure is the **deployment name**, not the model's name. The
  preset ships the literal placeholder `your-deployment-name` to make that obvious.

Because the `auth*` surface is protocol-shaped rather than vendor-shaped (§4),
moving from a direct endpoint to a gateway is a configuration change, not a code
change — but whether it works at all is the customer's gateway policy to state. This
is question 3 in the customer questionnaire in §…, and it cannot be answered from our
side.

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

## 3c. Gateway deployment mode (APIM / LiteLLM)

**Assume the gateway is the enterprise norm, not the exception.** Enterprises put a
gateway in front of Azure OpenAI because Azure OpenAI has no per-user accounting — it
cannot tell you which human spent a token. Per-user quotas, chargeback, model
routing, content policy and audit all live at the gateway. For our target buyer this
is likely the *default* deployment, with the direct endpoint being the small-customer
case.

### This makes Entra sign-in more valuable, not less

APIM's token governance is keyed by a **policy expression**, and the per-user case
requires the token to carry a user identity:

```xml
<llm-token-limit tokens-per-minute="50000" estimate-prompt-tokens="true"
  counter-key="@(context.Request.Headers.GetValueOrDefault("Authorization","").AsJwt()?.Claims.GetValueOrDefault("preferred_username","anonymous"))"
  tokens-consumed-header-name="consumed-tokens"
  remaining-tokens-header-name="remaining-tokens" />
```

A static API key can only ever be keyed by `context.Subscription.Id` — per-app or
per-team, never per-user. **Per-user token governance is only expressible if the
client sends a user token.** Entra sign-in is therefore what makes the customer's
quota policy possible, rather than an obstacle the gateway works around.

Land mine: `counter-key` must yield a distinct value per caller or all users silently
share one budget. Prefer `oid` over `preferred_username`.

### What the gateway checks, and with which audience

APIM's documented AI pattern is layered, and it does *not* trust our token blindly:

1. `validate-azure-ad-token` / `validate-jwt` — validates signature, issuer,
   **audience** and claims. Microsoft's guidance is to create an app registration
   *to represent the AI API* in APIM, so the expected audience is commonly
   `api://<their-gateway-app>`, **not** `cognitiveservices.azure.com`.
2. `llm-token-limit` — per-caller token rate and quota (429 on rate, 403 on quota).
3. `authentication-managed-identity` — APIM calls Azure OpenAI as itself; the backend
   credential never reaches us.

So the audience is **customer configuration**, and on our side it is an `authScope`
change rather than a code change — §4 already models it as a protocol-shaped axis.
Consequence: `AADSTS650052` (§1b) is a **direct-path** fence. A gateway customer
consents to *their own* app instead and never hits it.

### Gap: the production pattern uses two credentials at once

Microsoft's documented production pipeline takes a **subscription key and a JWT
together** — key first (no external call), then the JWT:

> A request arrives with a subscription key and a JWT. APIM validates the key first
> (fast, no external call), then validates the JWT against Azure AD, then forwards
> the request to Azure OpenAI using its Managed Identity token.

Our current model cannot express that: `authMode` is single-valued
(`apiKey` | `oidc` | `none`) and `authHeaderName` / `authHeaderPrefix` describe exactly
one header. `Ocp-Apim-Subscription-Key` **plus** `Authorization: Bearer` has nowhere
to live.

This is a design gap rather than a bug, and it is cheap to close now — an optional
additional static header applied alongside whatever the token provider produces.
Decide **before Phase 3**, since the UI must be able to express it. Note APIM only
requires a subscription key when the API sets `subscriptionRequired=true`; a
JWT-only gateway is also valid and some customers will run that.

### Who needs a role, and how that scales

A recurring worry when this is explained: *"so an admin has to assign a role to every
user?"* No — but the answer differs by architecture, and it deserves stating
explicitly because it is the first thing a customer's platform team asks.

**Role assignments are not per user.** RBAC takes a *principal*, and the usual
principal is a **group**. One assignment to one group covers everyone in it; membership
is then ordinary IT work, usually automated (Entra dynamic groups, or synced from the
existing onboarding flow). In practice it is a line of infrastructure-as-code —
`az role assignment create … --assignee <group>` — not a ticket per person. And
"everyone" is rarely the whole organisation: typically a department or a pilot group.

Note also *which* roles these are. Control plane (Owner/Contributor) is held by a
handful of administrators. Data plane (`Cognitive Services OpenAI User`) is held by the
user population. Two different job families, deliberately, so that billing admin does
not imply the ability to read prompts.

**In the gateway architecture the problem largely disappears.** APIM calls Azure OpenAI
with its *own* managed identity, so the only data-plane assignment required is that one
identity. Users are authorised by APIM's policy instead, and their quotas keyed on
claims inside their token.

| | Direct to Azure OpenAI | Through an APIM gateway |
|---|---|---|
| Who holds data-plane RBAC | each user, via a group | APIM's managed identity only |
| Assignments to maintain | one per group | exactly one |
| Per-user token quotas | not possible natively | yes, keyed on token claims |
| Onboarding a user | group membership | nothing on the Azure side |

So the per-user governance benefit and the per-user entitlement cost land in the same
architecture. A gateway buys the governance **and** removes the role sprawl — which is
the real reason enterprises run one, and a strong argument for the §3c design.

### First gateway target, and the order to build it in

"The gateway" is three different integrations, and they cost very different amounts of
work. Naming them avoids building the wrong one first.

| Variant | What it expects | What JASP needs | Code? |
|---|---|---|---|
| **A. Same audience** | validates `aud=https://cognitiveservices.azure.com`, calls the backend with its own managed identity | **endpoint URL only** | no |
| **B. Own audience** | a scope from the customer's own app registration, e.g. `api://their-app/access_as_user` | `authScope` change | no |
| **C. Audience + subscription key** | a JWT **and** `Ocp-Apim-Subscription-Key` | a second, static header | **yes** |

**Start with A.** It exercises the entire gateway path — JWT validation, per-user quota
keyed on a token claim, managed-identity backend auth — while asking nothing of us
beyond an endpoint change. It proves the architecture before we touch code.

#### Correction: C is *not* the enterprise target

An earlier draft of this document claimed C (token **plus** subscription key) was the
enterprise target. Researching it changed the picture, and that claim was wrong:

- **Microsoft's own reference implementation supports Entra-only, with per-user
  accounting.** `microsoft/AzureOpenAI-with-APIM` states its policy *"only uses the APIM
  subscription as the token counter when it exists. When the API is called without an
  APIM subscription, the policy falls back to a counter key derived from forwarded
  identity data"*, and emits dimensions for End User ID, End User Tenant ID and Client
  Application ID. **Per-user token quota therefore does not require a subscription key.**
- **Microsoft advises moving off subscription keys.** Same repo: *"using the Subscription
  key is straightforward but we recommend moving from the Subscription key to Managed
  Identities beyond development."*
- **Disabling the key is a first-class documented setting**, not a hack. APIM's
  subscription docs describe turning off **Subscription required** at API or product
  level, after which "the selected API or APIs can be accessed without a subscription
  key".
- **What the key actually buys is team/product attribution, not user identity.** The
  token supplies the person; the key supplies the cost centre.

It also carries a cost we should be reluctant to accept: **C puts a shared secret back
into every client install** — the very thing this feature exists to remove. A per-team
key distributed to every JASP install is a smaller version of the same problem.

So: **Entra-only (A/B) is the target; C is compatibility.** Build C because customers run
it and Microsoft documents it — not because it is where governance lives.

> APIM caveat: enabling keyless access *without* a JWT policy leaves the API
> anonymously reachable. Microsoft's subscription docs flag exactly this —
> *"configurations that could potentially enable unintended, anonymous API access."*
> A and B are only safe when `validate-jwt` / `validate-azure-ad-token` is present.

#### How a scope becomes a token

A scope is written `{resource}/{permission}` and answers two questions: **which API**, and
**which permission**. Entra resolves the resource half into the token's `aud` claim.

| Scope requested | Resource | Resulting `aud` |
|---|---|---|
| `https://cognitiveservices.azure.com/.default` | Microsoft's Cognitive Services API | `https://cognitiveservices.azure.com` |
| `api://their-gateway/access_as_user` | the customer's own app registration | that app's client ID |

`.default` is an Entra shorthand for *"every permission this client is already consented for
on that resource"* — used when enumerating them individually buys nothing.

**Nothing else in the flow changes between setups 2 and 3.** Same system browser, same PKCE,
same loopback handler, same token exchange. Only the scope string differs — and consequently
the `aud` of the token that comes back. On our side that is one field, `authScope`, which
already exists.

**The wrinkle is consent.**

- **Setup 2:** the permission lives on Microsoft's app, and JASP declares it in
  `requiredResourceAccess`. Consent is a one-off we control — the fence already climbed.
- **Setup 3:** the permission lives on a *customer's* app, which we cannot declare per
  customer. Consent therefore has to happen at sign-in, via Entra's **dynamic consent**: the
  client requests a scope it has not pre-declared and the user (or an admin) is prompted. If
  tenant policy requires admin consent, sign-in fails with `AADSTS65001`.

Note the shape of that constraint: `/adminconsent` consents only to what appears in
`requiredResourceAccess`, so a dynamic scope cannot be pushed tenant-wide that way. The
customer admin grants it either by signing in once themselves, or through **Enterprise
applications → the JASP service principal → Permissions**.

> Unverified: that dynamic consent behaves cleanly for a *foreign* multi-tenant public client
> requesting a scope on a customer's single-tenant resource app. The mechanism is documented
> and standard, but it has not been exercised against a real tenant in this project. Verify
> before promising a customer that setup 3 is drop-in.

**Why a customer would choose 3 anyway:** it binds the gateway to its own approved clients. A
`cognitiveservices`-audience token (setup 2) can be minted by any client the tenant consents
to that API; a token for *their* app can only be minted by clients they approved. Tighter, at
the cost of more setup.

**B needs research before it needs code.** Our client is a multi-tenant public client
with a fixed client ID. Asking for a token for a *customer's* API requires their
resource scope to be reachable from our registration, which is not something we can
declare per customer. Verify the mechanics against a real tenant before designing
around them — and note that a gateway customer on variant B consents to *their own*
app, so `AADSTS650052` never applies to them.

**What a gateway test needs on the Azure side** — all customer-side, none of it ours:

- An APIM instance. Tiers differ in which AI-gateway policies they support, so check
  before committing; prefer the newer v2 tiers over Developer on cost.
- The Azure OpenAI resource imported as an API (a one-click import exists).
- APIM's **managed identity** granted `Cognitive Services OpenAI User` on that
  resource — the single assignment that replaces per-user RBAC entirely.
- An inbound policy: `validate-azure-ad-token` → `llm-token-limit` (keyed on
  `@(…AsJwt().Claims["oid"])`) → `authentication-managed-identity`.
- Optionally **disable** the subscription requirement, so shape A needs no key at all.

### Which APIM tier to pick

Verified 2026-09 against Microsoft's policy reference and tier feature tables.

| What we need | Consumption | Developer | Basic v2 | Standard v2 |
|---|---|---|---|---|
| `validate-azure-ad-token` / `validate-jwt` | ✅ | ✅ | ✅ | ✅ |
| `authenticate with managed identity` | ✅ | ✅ | ✅ | ✅ |
| `llm-token-limit` | ❌ | ✅ | ✅ | ✅ |
| `rate-limit-by-key` / `quota-by-key` | ❌ | ✅ | ✅ | ✅ |

**Which tier depends on what you are testing, and the price gap is large.** Prices are per
unit at 730 h/month, list, East US, from the Azure Retail Prices API (2026-08):

| Tier | Price | Auth policies | `llm-token-limit` | Notes |
|---|---|---|---|---|
| Consumption | **free** to 1M ops/month, then ~$3.50/million | ✅ | ❌ | serverless, scales to zero, no SLA |
| Developer | **~$0.066/hr ≈ $48/month** | ✅ | ✅ | classic, no SLA, slow to provision |
| Basic v2 | **~$0.205/hr ≈ $150/month** | ✅ | ✅ | fast provisioning, 99.95% SLA |
| Standard v2 | **~$0.959/hr ≈ $700/month** | ✅ | ✅ | 50M requests included, $2.50/million over |
| Premium v2 | ~$3.84/hr ≈ $2,800/month | ✅ | ✅ | 99.99%, zones, VNet injection |

v2 tiers bundle a request allowance into the unit price (Basic v2 10M, Standard v2 50M); classic
dedicated tiers are flat per unit with no metered bundle. **Scale-out units are cheaper than the
first** — Standard v2's are ~$0.685/hr (~$500/month).

**So the right tier depends on the goal, and an earlier draft of this section got it wrong.**

- **Testing only the auth path** — which is all shapes 2 and 3 actually need — use
  **Consumption**. It supports `validate-azure-ad-token` and managed identity, scales to zero,
  and test traffic is a handful of requests, so it is effectively **free**. The earlier
  instruction to avoid Consumption was about *governance*, not about this test.
- **Testing token limits as well** — use **Developer at ~$48/month**, which supports the full
  policy set. Basic v2 at ~$150 buys faster provisioning and an SLA, neither of which a test
  needs.
- **Standard v2 (~$700/month) is not what you want for a test.** It is a production tier.

> **"Microsoft Entra integration" in APIM's tier tables is not what it sounds like.** The
> pricing page qualifies the row as *"Azure Active Directory integration **in developer
> portal**"* — it concerns the API catalogue portal's own sign-in, not token validation on API
> requests. It shows ❌ for Consumption and Basic, which is a red herring for us: the policies we
> need — `validate-azure-ad-token`, `validate-jwt`, `authenticate with managed identity` — are
> listed as supported on **all** gateways, Consumption included. Do not upgrade a tier because
> of that row.

There is **no in-place migration from classic to v2** — v2 is create-new only — so a throwaway
Developer instance costs nothing in future flexibility. And **delete it when finished**:
provisioned tiers bill hourly whether or not traffic flows.

Behaviour differences that matter more than the policy text:

- **Classic tiers use a sliding window; v2 tiers use a token bucket.** In v2, every policy
  instance sharing a `counter-key` must use the *same* `tokens-per-minute` value, or behaviour
  becomes unpredictable.
- **Counters are per-gateway** — not aggregated across regions or workspace gateways, so a
  multi-region deployment enforces limits independently per region.
- Exceeding `tokens-per-minute` returns **429**; exhausting `token-quota` returns **403**.
- With `estimate-prompt-tokens="false"` the limit is only known *after* the backend responds,
  so requests can reach the model before a limit is detected.

**The new "AI Gateway tier" (public preview) is not suitable for testing shapes 2/3.** It
provisions in about a minute, needs no scale units, and configures policies as portal cards
rather than XML — all attractive. But applications call it *"with a runtime access key"*, i.e.
it is key-based by design, and it exists only in East US 2 and Sweden Central. Worth watching;
not usable for a JWT test today.

**Scope differs between the two Azure surfaces** — worth knowing before debugging:

| Surface | Token scope |
|---|---|
| Azure OpenAI resource (`*.openai.azure.com`) | `https://cognitiveservices.azure.com/.default` |
| Microsoft Foundry project endpoint | `https://ai.azure.com/.default` |

The shipped preset uses the former, correct for an Azure OpenAI resource. A customer
pointing at a Foundry project endpoint needs the latter — configuration, not code.

### Supported shapes, and what each needs from us

Stepping back: the industry does not use one pattern, and "gateway" names several
different products. What matters is that from JASP's side they are all the same shape —
an endpoint, an audience, and a header. Support is therefore configuration, not
integration.

| Deployment shape | `endpoint` | `authMode` | `authScope` | Code needed? |
|---|---|---|---|---|
| Azure OpenAI / Foundry, direct | `https://<res>.openai.azure.com/openai/v1/…` | `oidc` | `https://cognitiveservices.azure.com/.default` | no — **works today** |
| APIM validating that same audience | APIM URL | `oidc` | unchanged | no |
| APIM publishing its own scope | APIM URL | `oidc` | `api://<their-app>/.default` | no — config, plus consent to *their* app |
| APIM requiring a subscription key **as well** | APIM URL | `oidc` | either | **yes** — the two-credential gap |
| Gateway that only wants a key (APIM key, LiteLLM virtual key, corporate proxy) | gateway URL | `apiKey` | — | no — **works today** |
| Corporate proxy with a custom key header | proxy URL | `apiKey` | — | no — `authHeaderName`/`authHeaderPrefix` cover it |

**Exactly one gap: a token and a second static header at the same time.** Everything
else in that table is configuration against surfaces that already exist.

Note how much is already covered by `apiKey`. Many gateways — APIM with a subscription
key, LiteLLM with virtual keys, a corporate reverse proxy — authenticate the *caller*
with their own key and do not want an Entra token at all. Those work today, and they
will stay common until a customer specifically needs *per-user identity*, which is the
one thing only a token can provide.

### What the field actually does

Researched 2026-09. Treat the market figures as directional (they are vendor and
market-research sourced); treat Microsoft's own guidance as the stronger signal, since
it describes what they see customers building.

- **Gateways are mainstream, and skew enterprise.** Roughly 42% of enterprises report
  using a middleware layer to manage AI infrastructure; the LLM-gateway segment is the
  largest slice of a market forecast to reach ~$11B by 2035, with large enterprises at
  ~79% of it. (SNS Insider via GlobeNewswire; getmaxim.ai — both vendor-affiliated, so
  read as trend rather than census.)
- **Microsoft documents the two-credential pattern explicitly.** Its AI-gateway
  guidance describes three layers: a **subscription key** for attribution and product
  scoping, **JWT validation** for identity, and **managed identity** for the
  gateway→model hop. The Unified AI Gateway design-pattern post puts it as *"supporting
  both API key and JWT validation for inbound requests, with managed identity used for
  backend authentication"*.
- **Key-only is legitimate too.** Microsoft's own sample repo
  (`Azure-Samples/AI-Gateway`) describes clients authenticating *"using APIM
  subscription keys **or** Entra ID"*. Entra-only is a supported path, not a workaround.
- **The client change is usually the endpoint alone.** Microsoft's guidance: apps using
  the OpenAI SDK *"only change the `api_base` endpoint to point to APIM"* — the variant
  A claim, confirmed by the vendor.
- **Microsoft's newest AI Gateway tier defaults to a runtime access key.** Key-based
  auth is therefore the baseline assumption in the ecosystem, and per-user Entra
  identity is the more advanced configuration. That is exactly the space JASP occupies.

**Reading:** the common shapes are already covered by existing configuration. The gap —
key **plus** token — is not an edge case; it is the pattern Microsoft documents for AI
APIs.

**Expected friction, worth knowing before you start:**

- **Path shape.** APIM's one-click "Import Azure OpenAI" traditionally publishes the
  *classic* surface — `/deployments/{name}/chat/completions?api-version=…` — whereas
  the shipped preset calls `/openai/v1/chat/completions`. Those do not line up. Either
  publish the v1 path on the gateway, or point `endpoint` at the gateway's own path and
  move `api-version` into `extraParams`.
- **Tenant pinning.** A gateway validating `tenant-id` decides who may call it. The
  preset's `authAuthority: organizations` accepts *any* work account, so a user can
  authenticate in a tenant the gateway will then reject. For a tenant-pinned gateway,
  set `authAuthority` to that tenant. Configuration, not code.
- **Tier.** The AI-gateway policies are not available on every APIM tier. Confirm
  support before choosing one.

### How per-user quota actually works

`llm-token-limit` maintains **one counter per distinct value of its `counter-key`** — a
policy expression — and compares that counter against `tokens-per-minute` and
`token-quota`. So "can I have per-user quotas?" is really "what does `counter-key`
evaluate to?":

```xml
<!-- Per subscription: needs a subscription key, because context.Subscription is only
     populated when a key was presented. -->
<llm-token-limit counter-key="@(context.Subscription.Id)" token-quota="100000" ... />

<!-- Per person: derived entirely from the token. No key required. -->
<llm-token-limit counter-key="@(context.Request.Headers.GetValueOrDefault(&quot;Authorization&quot;,&quot;&quot;).AsJwt()?.Claims.GetValueOrDefault(&quot;oid&quot;,&quot;anonymous&quot;))" ... />
```

The key and the token are two **sources for the same counter**, not two credentials
serving two purposes. Key → per team/product. Token → per person. Microsoft's own
sample uses the subscription when present and falls back to identity claims when not.

> Land mine: if `counter-key` degenerates to a constant — a missing claim defaulting to
> `"anonymous"` — **every user silently shares one budget**. Prefer `oid`, and verify
> the claim is actually present before trusting a quota.

> **Land mine, found live on 2026-09-21:** `authentication-managed-identity` **overwrites the
> `Authorization` header**. It must be placed **after** `llm-token-limit`, never before. If it
> runs first, the limiter parses APIM's own managed-identity token, `oid` resolves to the
> *service principal's* object ID, and every user shares one budget — with no error to indicate
> it. Policy order in the inbound section is therefore load-bearing:
> `validate-azure-ad-token` → `llm-token-limit` → backend/path → `authentication-managed-identity`.
> The order-independent alternative is to have `validate-azure-ad-token` output the token
> (`output-token-variable-name`) and key the limiter off that variable.
>
> ✅ Confirmed working end to end against a live APIM instance — see
> `10_apim_test_checklist.md`. `llm-token-limit`'s enforcement was proven by setting
> `tokens-per-minute="10"` and observing APIM's own `429`.

### LiteLLM specifics

LiteLLM supports Entra properly, but the support is **Enterprise-licensed**:

- **JWT auth** (`enable_jwt_auth`, `JWT_PUBLIC_KEY_URL` → Entra's
  `…/{tenant}/discovery/v2.0/keys`, optional `JWT_AUDIENCE` / `JWT_ISSUER`). Enterprise.
- **JWT → virtual-key mapping**, giving each JWT client its own budget, RPM/TPM limits,
  model allowlist and spend tracking *without issuing API keys*. Enterprise.
  `unregistered_jwt_client_behavior` is `fallback_team_mapping` (default), `reject` or
  `auto_register`.
- **SCIM provisioning** from Entra. Enterprise. SSO for the admin UI is free to 5 users.
- Entra App Roles can drive LiteLLM roles. The stable user claim for Entra is **`oid`**
  (Okta and most other providers use `sub`).

Two ways JASP interoperates, both configuration:

| Their LiteLLM is configured with | JASP needs |
|---|---|
| `JWT_AUDIENCE` = a scope they publish | `authScope` set to that scope; consent to their app |
| audience validation disabled | **nothing** — our existing `cognitiveservices`-audience token is accepted |

**Worth knowing: Claude Desktop solves this exact problem the same way.** Anthropic's
guide for running Claude Desktop against an enterprise gateway specifies an OIDC
authorization-code-with-PKCE sign-in **in the system browser**, the resulting token sent
as `Authorization: Bearer` on every request, and the gateway mapping users on `oid` for
Entra (`sub` for Okta). LiteLLM, Kong, Envoy and APIM are all named as supported
gateways. That is our architecture, arrived at independently — and a useful spec to
compare against while refining Phases 3 and 5.

### Parity target: Claude Desktop's gateway surface

Anthropic ships this same feature and publishes its configuration surface. Adopting it
as our target is cheaper than inventing one, and it means a customer who has already
onboarded Claude Desktop can onboard JASP the same way.

| Their key | Ours | Status |
|---|---|---|
| `clientId` | `authClientId` | ✅ have |
| `issuer` (base URL; app appends `/.well-known/openid-configuration`) | `authAuthority` | ✅ have |
| `scopes` (default `openid profile email offline_access`) | `authScope` + those three implicit | ✅ have |
| `authorizationUrl` / `tokenUrl` | — | ❌ **missing** — for IdPs serving no discovery document |
| `redirectPort` (fixed loopback port) | — | ❌ **missing** — we always take an ephemeral port; Okta requires an exact match |
| `appendOfflineAccess` | hardcoded | ❌ **make configurable** — some IdPs reject `offline_access` |
| `bearerTokenType` — `id_token` (default) \| `access_token` | access token only | ❌ **missing — and the interesting one** |
| `resource` (RFC 8707) | — | not needed — Entra rejects the parameter; AD FS only |
| `additionalRedirectReferrerHosts` | — | not applicable to our callback handling |

**`bearerTokenType` deserves attention.** Claude defaults to sending the **ID token**, not
the access token. An id_token's `aud` is *the client's own ID*, and it still carries
`oid`, `tid` and `preferred_username`. That lets a gateway be configured as a plain OIDC
client:

- LiteLLM: `JWT_PUBLIC_KEY_URL` = Entra's JWKS, `JWT_AUDIENCE` = the client ID. Done.
- The customer never has to publish their own API scope, and the gateway never needs to
  know `cognitiveservices.azure.com` exists.

That is materially **less** customer configuration than the access-token path, which is
presumably why Anthropic default to it. Direct-to-Azure-OpenAI still needs the access
token, because `aud` must be `cognitiveservices.azure.com` — so it is a per-deployment
setting, exactly as they model it.

Also worth noting: they **ship the OS broker** as an option for Conditional Access
policies that demand a compliant device, rather than treating it as a blocker. §3b rejects
shipping it on licence grounds and that analysis stands — but a comparable commercial
product ships it, so the licence position is worth re-examining before we tell a customer
we cannot serve them.

### Who owns the client app — and why Claude has the customer register it

This decides how much friction a customer actually feels, and it is easy to miss.

Our shipped default is **JASP's own multi-tenant registration**: the customer registers
nothing and instead consents to a vendor app appearing in their tenant.

Claude Desktop does the **opposite**, and their setup steps repay careful reading. The
customer registers an app in their *own* tenant:

- **Accounts in this organizational directory only** — single-tenant
- platform **Mobile and desktop applications**, redirect `http://127.0.0.1/callback`
- **no client secret, no API permissions**

…and then pastes **that app's client ID** into the desktop configuration. The gateway validates
`audience: YOUR_CLIENT_ID` — correct, because in `id_token` mode the audience *is* the client ID.

So in their model **no vendor application exists in the customer's tenant at all.** Three
consequences worth taking seriously:

- **No cross-tenant consent** — therefore none of the `AADSTS650052` / `AADSTS700016` /
  missing-service-principal failures that consumed a day here.
- **No vendor-trust review** of an application the customer did not write. The app that signs
  users in is theirs, owned and revocable by them.
- **Nothing depends on the vendor's registration remaining valid** — no client-ID rotation,
  and no publisher-verification dependency on this path.

**Do not conclude that single-tenant is "better".** Their app is single-tenant *because it is
the customer's* — it only ever has to serve one directory. **Our shared registration must stay
multi-tenant**: a single-tenant registration cannot be consented to by any other tenant, which
would break shape 1 entirely. The two models differ in *who owns the app*, not in a tenant-policy
preference.

A useful consequence of their design: in `id_token` mode the token's `aud` is the **client ID
itself**, so one registration serves as both client and audience. There is no separate resource
scope, and therefore no `AADSTS65001` consent step. That also means **a gateway customer on this
path needs no `authScope` at all** — just a client ID, an authority and an endpoint. It largely
dissolves shape 3's dynamic-consent question, because a customer's own app needs no foreign
consent.

**APIM and BYO are independent axes.** A gateway (or not) says *where requests go*; BYO (or not)
says *whose app signs the user in*. All four combinations are valid:

| | Our shared app | BYO app |
|---|---|---|
| **Direct to Azure OpenAI** | shape 1 — the one thing tested | needs a gateway to be meaningful |
| **Behind APIM** | shapes 2 / 3 | endpoint + `authClientId` + `id_token` |

The useful consequence: **if a customer wants shape 3's property — a gateway that accepts only
tokens minted for its own app — BYO is the easy route to it.** Shape 3 with our shared app needs
a second resource app *and* foreign-client dynamic consent (the unverified mechanism above). BYO
gets the same property from a single registration the customer already owns, with no scope and no
foreign consent. One app instead of two, and the uncertain mechanism drops out.

So: **APIM does not require BYO.** But a customer who has already decided to run APIM has, by
doing so, already accepted central configuration — which is the only thing BYO actually costs.

The cost is that every install has to be told the client ID — which is precisely why managed
configuration matters: the customer registers once, then pushes the value.

**We already support this.** `authClientId` is a per-provider field defaulting to JASP's
registration; pointing it at the customer's own app is configuration, not code.

**Recommendation:** support both, and document the bring-your-own-app path as the enterprise
answer. More setup for the customer's platform team, but it removes the conversation that
usually takes longest — *"why is this vendor's application in our directory?"*

#### Closing the gaps — and the Qt APIs that already do it

Verified against the Qt 6.11.2 sources in `C:\Qt\6.11.2\Src\qtnetworkauth\src\oauth\`.
Nothing here needs a new dependency:

| Claude key | Qt API | Where |
|---|---|---|
| `bearerTokenType` | `QAbstractOAuth2::idToken()` / `idTokenChanged` | `qabstractoauth2.h:163` |
| `authorizationUrl` / `tokenUrl` | `QAbstractOAuth::setAuthorizationUrl()`, `QAbstractOAuth2::setTokenUrl()` | `qabstractoauth.h:100`, `qabstractoauth2.h:166` |
| `redirectPort` | `QOAuthHttpServerReplyHandler(quint16 port, QObject *)` | `qoauthhttpserverreplyhandler.h:29` |
| callback path `/callback` | `setCallbackPath()` | `qoauthhttpserverreplyhandler.h:37` |
| `appendOfflineAccess` | we build the scope set ourselves | `browsertokenprovider.cpp:294` |
| `inferenceSessionLifetimeSec` | `QAbstractOAuth2::expirationAt()` | `qabstractoauth2.h:146` |

**Adopt a custom-headers map — but not as a place for secrets.** Claude's
`inferenceCustomHeaders` is a generic key: extra headers on every inference request. Read
its constraint, though, which is stricter than it first appears:

> *Extra headers on every inference request — routing and tenant headers only (org IDs,
> Bedrock Guardrails). **No credentials**; use the credential helper for tokens.*

> *Do not put API keys, bearer tokens or other credentials here — this map is stored and
> distributed as plain configuration.*

So it does **not** carry an APIM subscription key. Secrets go through the credential-helper
script, which prints `{"token": "…", "headers": {"Name": "Value"}}` and whose headers are
merged over the static ones. But `inferenceCredentialKind` is single-valued — `static`,
`helper-script`, `interactive`, `vendor-profile`, `workforce`, and *"when set, only that
source is used (no fallback)"* — so **the helper cannot be combined with interactive
sign-in**.

**Net: Claude cannot do interactive SSO plus a secret subscription key either.** Their
`inferenceGatewayAuthScheme` is `bearer` **or** `x-api-key` (a replacement for
`Authorization`, never an addition), and their header map is non-secret by policy. The
leading commercial design in this space pushes you away from the two-credential
combination as well — toward the Entra-only shape APIM already supports. That is the
strongest argument yet for treating shape 4 as *avoidable* rather than as a target.

We should still ship a custom-headers map for the non-secret routing cases, and for parity
— but it closes *routing* headers, not the subscription-key gap.

#### Claude's configuration is managed — and so is JASP's

*Correction: an earlier draft called managed configuration a JASP gap. It is not.*

Every key above is delivered as **OS-native managed configuration**, and local values are
ignored whenever a managed source is present:

| Platform | Managed location | Local (user) location |
|---|---|---|
| Windows | `HKLM\SOFTWARE\Policies\Claude` (machine), `HKCU\SOFTWARE\Policies\Claude` (user) | `%LOCALAPPDATA%\Claude-3p\configLibrary\` |
| macOS | `.mobileconfig` in `/Library/Managed Preferences/` | `~/Library/Application Support/…` |
| Linux | `/etc/claude-desktop/managed-settings.json` (root-owned, not group- or world-writable) | `~/.config/Claude-3p/configLibrary/` |

Plus two remote paths: an Anthropic-hosted **admin console**, and a **bootstrap server** —
an HTTPS endpoint returning per-user JSON that overrides local settings (*"values from the
response override local settings and become read-only"*), with `trustBootstrapDelivery`
gating whether users are prompted to consent to what it delivers. There is a configuration
re-check interval (10 minutes by default), a relaunch-enforcement window, and per-field
deprecation warnings.

`Settings::value()` already implements the same precedence Claude does
(`Desktop/utilities/settings.cpp`):

```cpp
// 1. Enterprise Machine Policy (Strict GPO from IT Admins)
QSettings gpoMachine("HKEY_LOCAL_MACHINE\\Software\\Policies\\JASP", QSettings::NativeFormat);
if (gpoMachine.contains(settingStringName)) return gpoMachine.value(settingStringName);
// 2. Enterprise User Policy  (HKEY_CURRENT_USER\\Software\\Policies\\JASP)
// 3. Current User Settings (Active INI)
// 4. Legacy Migration (Old MSI User Preferences in HKCU)
// 5. Fallback to hardcoded application defaults
```

And `AIConfigModel::loadUserData()` reads through that same accessor
(`aiconfigmodel.cpp:1078`):

```cpp
QString json = Settings::value(Settings::AI_USER_PROVIDERS).toString();
```

`AI_USER_PROVIDERS` is an ordinary `Settings::Type`, so **an administrator can already push
the entire AI provider configuration by machine policy**, with no new code. Unset, it falls
through to the user's own settings — the same "managed wins, local ignored" rule Claude has.

There is more: `Desktop/gui/jaspConfiguration/` implements a **`conf.toml`** local file plus
a **remote configuration URL** that overrides it and is cached locally for offline use, with
a parser factory (`Format::TOML`) built to accept further formats. Today it carries modules,
analysis options, runtime constants and startup commands — not AI settings — but it is the
right home for them.

**So managed configuration is a configuration-surface task, not new infrastructure.** Three
caveats to record:

- **GPO is Windows-only.** macOS and Linux enterprise deployments need the `conf.toml` route
  extended, since that file can be deployed by any MDM on any platform. Worth doing for
  parity regardless of platform mix.
- **Policy overrides rather than merges.** A policy-set `aiUserProviders` replaces the user's
  blob entirely, so an administrator pinning an endpoint also wipes a user's own provider
  additions. Decide deliberately whether merge semantics are wanted. Related: any setting
  with no `Settings::Type` cannot be policy-overridden at all.
- **Do not put a shared key in machine policy.** `HKLM` policy values are readable by every
  user on the machine. Entra sign-in configuration is secret-free and therefore a perfect fit
  for policy; an API key is not.

#### Bearer-token type is a real architectural fork

Claude defaults to the **ID token**, and their gateway guidance shows what that buys. For
LiteLLM the whole configuration is `public_key_url` + `audience` = the **client ID** +
`user_id_jwt_field: oid`. No customer-published API scope, no consent to the customer's own
app, no `AADSTS65001`.

In access-token mode their docs note *"in `access_token` mode also grant the gateway API's
delegated permission, or sign-in fails with `AADSTS65001`"* — which is exactly shape 3's
consent requirement. **Defaulting to the ID token makes the customer's setup materially
smaller**, which is presumably why it is their default.

For us: direct-to-Azure-OpenAI needs `aud=cognitiveservices.azure.com`, so `access_token`
stays the default for that preset; gateway presets should default to `id_token`.

#### Two details worth copying wholesale

- **`appendOfflineAccess` has a subtlety we currently get wrong.** In `id_token` mode with
  `scopes` set explicitly, Claude deliberately does **not** append `offline_access` (OIDC
  Core 11 treats it as requiring explicit consent), so silent refresh degrades to hourly
  re-prompts unless the administrator includes it. We always append it. Copy their rule and
  surface the consequence, because the failure is invisible until a session outlives an hour.
- **Their `Token exchange failed (HTTP 401)` troubleshooting entry** — the IdP app was
  registered as a confidential Web client instead of a public/native one. We will see this
  from customers, and it deserves its own message rather than a generic auth failure.

#### Redirect URI: their configuration and ours disagree — ours is verified

Claude requires `http://127.0.0.1/callback` and states `127.0.0.1` is mandatory (*"use
127.0.0.1 (not localhost), include the /callback path"*). Our verified working setup uses
`http://localhost` with no path, and Appendix A records that `127.0.0.1` was rejected.

Both may be valid — possibly depending on whether a path is present, or Entra's behaviour may
have changed. **Do not "fix" our working configuration to match theirs.** If we adopt a
`/callback` path for parity, re-register and re-verify first; a mismatch here fails with
`AADSTS50011` and costs an afternoon.

#### The broker, revisited

Claude ships broker sign-in (`inferenceGatewayOidcAuthFlow: broker`) and states the
justification plainly: *"Broker mode mints a token in the customer's own Entra tenant with
the customer-configured scopes, and forwards it to the customer's own gateway; both endpoints
of that trust relationship are inside the customer's control."*

That is a reasonable argument, and it is **not** the one §3b evaluated. §3b asked whether the
*binary* is licensable for redistribution; Anthropic lean on the *data path* staying
customer-controlled. Those are different questions, and the second may be answerable even if
the first is not. Worth putting to Microsoft before we tell a customer we cannot serve them.

Note our app registration already carries
`ms-appx-web://Microsoft.AAD.BrokerPlugin/fc57bc92-…` in its redirect URIs (§1b) — the Windows
broker redirect Claude requires is already registered.

### If the customer runs LiteLLM

LiteLLM is a self-hosted Python proxy, so a customer running it is running a service. Worth
knowing what that involves, since it is a likely answer to "what gateway do you have?"

**Deployment paths** (per LiteLLM's own production docs):

- **AKS + Helm is the officially supported Azure path.** Their docs are explicit: AWS and GCP
  have official Terraform modules, *"Azure has no Terraform module, so AKS with Helm is the
  supported path there"*.
- **Azure Container Apps** is not officially documented but has community `azd` templates
  (e.g. `build5nines/azd-litellm`) that stand up LiteLLM + PostgreSQL Flexible Server with
  `azd up`. Far faster to stand up than AKS; good enough for a test.

**Required components** — this is what makes it heavier than APIM operationally:

| Component | Why | Azure service |
|---|---|---|
| PostgreSQL | virtual keys, teams, budgets, spend logs — required for auth and tracking | Database for PostgreSQL Flexible Server |
| Redis | distributed rate limiting and cache; **required once >1 replica** | Azure Cache for Redis / Managed Redis |
| Migrations job | applies schema changes once per upgrade; proxies run `DISABLE_SCHEMA_UPDATE=true` | — |
| Secret store | master key, salt key, provider credentials | Key Vault via CSI driver |
| Ingress | TLS termination | Application Gateway Ingress on AKS |

Operational traps worth passing on: **`LITELLM_SALT_KEY` encrypts provider credentials in the
DB and must never be changed** — rotating it turns every stored credential into garbage and the
proxy fails to start. Pin image tags. `LITELLM_MODE: PRODUCTION` disables the `.env` lookup.

**Entra ID: two different features, and the licence split is the decisive bit.**

| Feature | What it does | Licence |
|---|---|---|
| SSO for the Admin UI | humans sign into the LiteLLM web UI via Entra | free to 5 users |
| **JWT auth for inference** | clients present an Entra JWT as the bearer token | **Enterprise** |
| JWT → virtual-key mapping | per-client budgets/limits from JWT claims | **Enterprise** |
| SCIM provisioning from Entra | users and teams synced from the IdP | **Enterprise** |

**So a customer only has shape 2/3 with LiteLLM if they hold an Enterprise licence.** Without
one, LiteLLM offers virtual keys — shape 5, which proves nothing about Entra. That is worth
asking early, because it also means such a customer is already paying for this capability.

Minimum config for JASP → LiteLLM:

```yaml
general_settings:
  enable_jwt_auth: true
  litellm_jwtauth:
    user_id_jwt_field: "oid"          # Entra's stable user ID, not sub
    user_email_jwt_field: "email"
    team_ids_jwt_field: "groups"      # optional, for team-level budgets
# plus JWT_PUBLIC_KEY_URL = https://login.microsoftonline.com/{tenant}/discovery/v2.0/keys
#      JWT_ISSUER       = https://login.microsoftonline.com/{tenant}/v2.0
#      JWT_AUDIENCE     = <client id>  (the app JASP signs in as)
```

That is the Claude-shaped configuration: `aud` is the client ID, because in `id_token` mode the
client ID *is* the audience — so a BYO-app customer needs no scope and no consent step.

**The backend hop is mostly keyless already.** LiteLLM can authenticate to Azure OpenAI with a
managed identity (`enable_azure_ad_token_refresh`) and to Azure Redis keyless. **Its own
PostgreSQL still needs a static password** — keyless DB auth is an open feature request
(`BerriAI/litellm#29661`), so a fully secret-free LiteLLM on Azure is close but not yet
available. Relevant if a customer's policy is "no standing secrets".

**One real-world pattern worth noting:** a published customer deployment
(`wukong121/secure-litellm-on-azure`) puts Front Door/WAF in front of a private AKS-hosted
LiteLLM and routes through a **self-built "Entra API proxy"** rather than relying on LiteLLM's
own JWT auth. So some organisations solve shape 3 themselves instead of buying the Enterprise
tier — expect to meet custom proxies in the field, and note they are shape 2/3 from our side
either way.

### Consequence for our own testing

The gateway path costs **more** Azure resources than the direct path, not fewer
(APIM instance + backend + its own app registration), and there is no cheap local
rehearsal. Closing out the direct path first remains the right order: it validates
the whole sign-in stack against a single fence, and it is a prerequisite for both.

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
- [x] Delegated `Microsoft Cognitive Services` → `user_impersonation` present in
      `requiredResourceAccess` (added via the manifest — the portal picker cannot
      offer an API the tenant has no service principal for, Appendix A)
- [x] Azure subscription + Azure OpenAI resource in the tenant — this is what created
      the service principal that made consent possible at all (§1b)
- [x] `user_impersonation` consented — **as user consent**; admin consent was never
      needed (§1b)
- [x] `Cognitive Services OpenAI User` on the resource, for the signed-in user (§3c)
- [ ] *(dropped)* Vendor `msalruntime` header + DLLs — superseded by §3b
- [ ] Stand up a free secondary test tenant for multi-tenant consent testing

### Phase 1 — Abstraction & config — ✅ DONE

- [x] `Desktop/auth/tokenprovider.h` — interface.
- [x] `Desktop/auth/apitokenprovider.{h,cpp}` — wraps `currentApiKey()`.
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

### Phase 2 — Browser backend — ✅ DONE (verified end-to-end)

- [x] `Desktop/auth/browsertokenprovider.{h,cpp}` —
      `QOAuth2AuthorizationCodeFlow` + `QOAuthHttpServerReplyHandler`, PKCE S256,
      system browser, `autoRefresh` + 5-minute `refreshLeadTime`.
- [x] `Qt::NetworkAuth` added to the Qt components and `JASPDesktopLib` link list.
- [x] `AiBridge`: provider selection by `authMode`/`authBackend`; the deferred
      request path; `signIn()` / `signOut()` / `isSignedIn()`;
      `authStateChanged()` / `authInteractionRequired()`.
- [x] **Build and run it.**
- [x] Redirect URI — logged on every attempt as
      `BrowserTokenProvider: sign-in redirect URI is http://localhost:<port>/`.
      `setCallbackHost("localhost")` is required: Qt's default advertises
      `127.0.0.1`, which the registration rejects (`AADSTS50011`).
- [ ] Retry once on HTTP 401. Deliberately **not** done: it touches the streaming
      teardown in `onReplyFinished()`, which is the riskiest code in the file, and the
      provider already renews ahead of expiry.

**Acceptance met:** a user signs in via the browser and completes an Azure OpenAI
call. Verified against resource `test888`. API-key providers are untouched —
`apiKey` keeps the original synchronous path.

Debugging additions made while chasing the 401, all worth keeping:

- `onReplyFinished()` surfaces the HTTP error **body** instead of discarding it, and
  `onReplyError()` stays quiet when the server already answered. Without this, Azure's
  `PermissionDenied` was overwritten by a generic "check your API key", which sent us
  down the wrong path more than once.
- `describeToken()` logs `oid` and `tid` alongside `aud`/`scp`, because a token for the
  wrong identity and a missing role are otherwise indistinguishable.

### Phase 3 — UI

- [ ] `PrefsAI.qml`: sign-in method selector; hide the API-key field when
      `authMode` is `oidc`; **Sign in / account / Sign out**; surface
      `authInteractionRequired` and auth errors.
- [ ] Show the signed-in account (`accountName()`) and expiry (`expiresAt()`).

### Phase 4 — Token persistence (OS vault)

- [x] `Desktop/auth/secretvault.{h,cpp}` — generic key→blob vault, **Windows
      Credential Manager backend** (DPAPI-protected, user-visible in Control Panel).
      macOS Keychain verified (2026-09-23); Linux stays in memory until libsecret
      is exercised — same behaviour as before, no regression.
      `SecretStore` is **not** used for refresh tokens (hardcoded `kMasterKeySeed` =
      obfuscation); it is the intended *fallback* backend for non-token secrets only.
- [x] `BrowserTokenProvider` persists the refresh token (plus the account name) per
      provider signature. On startup a stored token renews silently via
      `QAbstractOAuth2::refreshTokens()` — no browser. A rejected token is removed
      from the vault and falls back to interactive sign-in; a network failure during
      renewal surfaces as an error instead of opening a pointless browser.
      The access token is deliberately not stored: JWTs can exceed the vault's 2560-byte
      blob limit, and minting a fresh one is one token-endpoint call.
- [ ] Sign-out / revocation UI that clears the vault entry deliberately.
- [x] macOS Keychain + Linux Secret Service backends (libsecret loaded at runtime via
      `QLibrary`, so a missing library is a clean "unavailable" rather than a packaging
      dependency). **macOS compiled and verified end-to-end 2026-09-23** — the Keychain
      backend needed `-framework Security` added to the `JASPDesktopLib` link list
      (`Desktop/CMakeLists.txt`); sign-in survives a JASP restart via silent renewal.
      Linux written but not yet run.
- [x] Degradation policy encoded in the API: `SecretVault::Degrade::Never` for token-class
      secrets (write fails, caller reports), `ToEncryptedSettings` default for secrets a
      user typed and can revoke. `SecretStore` renamed to `EncryptedSettingsStore` and
      demoted to that fallback.
- [x] **Migrate API keys onto `SecretVault`** (the fallback's consumer). Keys now live at
      `JASP/AI/provider/<hash>` with the default degrade policy, so machines without an OS
      vault behave exactly as before — via the fallback. A one-time migration at the end of
      `loadUserData()` moves any legacy key out of the `aiUserProviders` JSON, and
      `resetToDefaults()` clears vault entries so a reset truly resets. `EncryptedSettingsStore`
      is now vault-internal (moved to `auth/`, its dead typed API removed).
- [ ] Flatpak packaging (build side, not this repo): add `--talk-name=org.freedesktop.secrets`
      to `finish-args` in `flatpak/org.jaspstats.JASP.json`; add libsecret as a pinned module
      only if the runtime lacks it. Note: this grant gives the sandbox access to all unlocked
      secrets in the user's keyring (no per-app ACL in the spec) — a documented, conscious
      choice, and Flathub's linter asks for justification.

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
- [x] macOS sign-in — **verified 2026-09-23**, including refresh-token persistence
      across restarts. Linux pending.
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
| `Desktop/auth/tokenprovider.h` | Interface | ✅ Phase 1 |
| `Desktop/auth/apitokenprovider.{h,cpp}` | Existing key behavior | ✅ Phase 1 |
| `Desktop/auth/browsertokenprovider.{h,cpp}` | System browser + loopback PKCE | 🚧 Phase 2, unbuilt |
| `Desktop/auth/devicecodetokenprovider.{h,cpp}` | Device-code fallback | Phase 5 |
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
   - Which **audience** does the gateway's `validate-jwt` expect —
     `cognitiveservices.azure.com`, or a scope published by an app registration of
     their own? (Drives `authScope`; see §3c.)
   - Does it also require a **subscription key**, or is the JWT sufficient? (Two
     credentials at once is not yet expressible — §3c.)
4. Is **per-user usage / chargeback** a requirement? (Azure OpenAI does not record
   caller identity — requires a gateway.) If yes, the gateway will key its token
   quota on a claim in our token, so confirm which claim it reads. `oid` is the safer
   key; `preferred_username` collides across some tenants.
5. **Data residency**, no-training confirmation, DPA. The customer expresses residency
   through the **deployment type**: `Data Zone Standard (US)`/`(EU)` keeps processing
   inside that zone while drawing capacity from across it; `Global Standard` does not
   guarantee a zone. All of these bill **per token**, so this is a policy choice for
   them, not a cost or capability difference for us — JASP only needs the endpoint.
   Only `Provisioned` changes the billing shape, and it is opt-in.

---

## Appendix A — Entra app registration checklist

| Setting | Value |
|---|---|
| Supported accounts (`signInAudience`) | `AzureADMultipleOrgs` (work/school only) |
| Platform | **Mobile and desktop applications** (not Web, not SPA) |
| `allowPublicClient` | `true` |
| Redirect URI (browser) | `http://localhost` — **port is ignored when matching** |
| Delegated permission | Microsoft Cognitive Services → `user_impersonation` (**required — currently missing**) |
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
- **The portal picker will not list "Microsoft Cognitive Services" on this tenant.**
  That is a chicken-and-egg artifact, not a missing setting: the *APIs my organization
  uses* tab only lists first-party APIs that already have a **service principal** in
  the tenant, and that service principal is created the first time someone consents to
  the API or stands up a resource of that type. A tenant with no Azure subscription
  has neither, so the API is absent from every search box in that blade — searching by
  name *or* by resource ID finds nothing. This does **not** mean an Azure resource is
  needed first.
- **Add it through the manifest instead**, which carries no such prerequisite: App
  registrations → `JASP AI Desktop` → **Manifest** → append to the existing
  `requiredResourceAccess` array (keep the Graph entries already present):

  ```json
  { "resourceAppId": "7d312290-28c8-473c-a0ed-8e53749b6d6d",
    "resourceAccess": [ { "id": "5f1e8914-a52b-429f-9324-91b92b81adaf", "type": "Scope" } ] }
  ```

  Save, then go back to **API permissions** → the row appears as *Not granted* →
  click **Grant admin consent** (Global Administrator, Privileged Role Administrator
  or Cloud Application Administrator). Consent is what provisions the resource's
  service principal; once it succeeds, `7d312290-…` also begins appearing in *APIs my
  organization uses*, which is a convenient confirmation that it took effect.
- CLI alternative if the manifest route is unavailable:
  `az ad sp create --id 7d312290-28c8-473c-a0ed-8e53749b6d6d` provisions the service
  principal directly, after which the API *does* show up in the picker. Use
  `az login --allow-no-subscriptions`, since the test tenant has no subscription.
- Delegated `user_impersonation` is the *only* permission this resource exposes — it
  has no application/daemon permissions at all, which suits the public-client design.
- Consent is not ownership. A tenant that subscribes to **no** Azure services can
  still consent and still receive a well-formed token. Actually *calling* a model
  additionally needs an Azure OpenAI resource, a model deployment, and the
  `Cognitive Services OpenAI User` role assigned to the signing-in user.

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
