# APIM gateway — verified configuration (shapes 2 and 3)

**Shape 2 was walked end-to-end on 2026-09-21.** This is the first gateway this project has ever
exercised: JASP signed in with a real Entra token, APIM validated it, keyed a token quota on the
user's identity, and exchanged it for a managed-identity token that `test888` accepted.

Everything below is the configuration that worked, in the order it was done, including the wrong
turns — they are the most useful part for the customer guide.

- **Shape 3** (gateway with its own scope) — still untested.
- **Per-user quota bucketing** — still unproven, see "What is not proven yet".

---

## Verified values

| Thing | Value |
|---|---|
| Azure OpenAI resource | `test888` → `https://test888.openai.azure.com` |
| Deployment name | `gpt-5.4-mini` |
| JASP client ID | `fc57bc92-9de6-405e-8a47-4161cc3e27d3` |
| Tenant ID | `1ef0aea6-4a04-4107-97f7-42fd5a5f8cfa` |
| Audience | `https://cognitiveservices.azure.com` |
| Scope (JASP) | `https://cognitiveservices.azure.com/.default` |
| Data-plane role | `Cognitive Services OpenAI User` on `test888` |
| APIM instance | `jasp-apim-dev` → `https://jasp-apim-dev.azure-api.net` |
| API name + suffix | `test888` |
| **JASP endpoint** | `https://jasp-apim-dev.azure-api.net/test888/chat/completions` |

---

## The topology that worked

```
JASP  --(Bearer: user's Entra token)-->  APIM  --(Bearer: APIM's managed identity)-->  test888
                                          |
                                          +-- validate-azure-ad-token   identity gate
                                          +-- llm-token-limit           per-user budget
                                          +-- set-backend-service       where it goes
                                          +-- rewrite-uri (operation)   path translation
                                          +-- authentication-managed-identity
```

Two tokens, two jobs. The user's token says **who is calling**; APIM's managed-identity token is
what **opens the resource**. No shared secret exists anywhere in the chain.

---

## Build steps, as performed

### 1. APIM instance

Portal → Create a resource → API Management. **Developer** tier, region near you, resource name
`jasp-apim-dev` (globally unique). Provisioning takes 30–45 minutes.

Developer bills hourly (~$0.066/hr) and has no SLA. It is the cheapest tier that has
`llm-token-limit`; Consumption is free for an auth-only test but lacks the quota policies.

### 2. Give APIM an identity

APIM → Security → Managed identities → **System assigned** → On → Save.

### 3. Let that identity use the model

`test888` → Access control (IAM) → Add role assignment → **`Cognitive Services OpenAI User`** →
Members: Managed identity → API Management → `jasp-apim-dev`.

> **Two traps.** The portal's managed-identity picker did not list "API Management" — the
> workaround was to paste the identity's **Object (principal) ID** from APIM → Security →
> Managed identities, with *Assign access to* set to *User, group, or service principal*.
> And role assignments propagate **lazily**: testing immediately gives a 401 that looks exactly
> like a policy bug. Wait 5–10 minutes.

### 4. Create the API

The **"Language models" import** was used. It works, but it has one consequence that cost most of
the debugging time:

> The import publishes the **OpenAI-native path surface** — `/chat/completions`, `/responses`,
> `/embeddings`, … — and creates **40 operations** plus a named backend
> (`test888-openai-endpoint`). Azure OpenAI's own native path is `/openai/v1/chat/completions`.
> The two do not line up.

So the import's API needs a path translation step (step 6). The alternative — defining the API
by hand with the real path — avoids the rewrite entirely, and is what a customer guide should
probably recommend.

The import also generated a policy with `counter-key="@(context.Subscription.Id)"`, i.e. quotas
keyed on the **subscription key**. That is Microsoft's default assumption; identity-keyed quotas
are the opt-in variant and had to be written by hand.

### 5. Turn the subscription key off

APIM → APIs → `test888` → Settings → uncheck **Subscription required**.

**Inferred, not directly observed:** this is off, because JASP sends no
`Ocp-Apim-Subscription-Key` header and the request still reached the backend. With the key
required, APIM would have answered `401 Access denied due to missing subscription key`.

Do this **with** step 6. A keyless API with no JWT policy is anonymously reachable — Microsoft's
own docs flag this configuration explicitly.

### 6. Policies — two scopes

**API scope** (`test888` → Design → the API-level Inbound processing → `</>`):

```xml
<policies>
  <inbound>
    <base />

    <validate-azure-ad-token tenant-id="1ef0aea6-4a04-4107-97f7-42fd5a5f8cfa"
                             failed-validation-httpcode="401"
                             failed-validation-error-message="Unauthorized">
      <audiences>
        <audience>https://cognitiveservices.azure.com</audience>
      </audiences>
      <client-application-ids>
        <application-id>fc57bc92-9de6-405e-8a47-4161cc3e27d3</application-id>
      </client-application-ids>
    </validate-azure-ad-token>

    <llm-token-limit
        tokens-per-minute="40000"
        estimate-prompt-tokens="true"
        counter-key="@(context.Request.Headers.GetValueOrDefault(&quot;Authorization&quot;,&quot;&quot;).AsJwt()?.Claims.GetValueOrDefault(&quot;oid&quot;,&quot;anonymous&quot;))"
        remaining-tokens-header-name="x-remaining-tokens" />

    <set-backend-service base-url="https://test888.openai.azure.com" />
    <authentication-managed-identity resource="https://cognitiveservices.azure.com" />
  </inbound>
  <backend><base /></backend>
  <outbound><base /></outbound>
  <on-error><base /></on-error>
</policies>
```

**Operation scope** — on the *"Creates a model response for the given chat conversation."*
operation only (its own Inbound processing → `</>`):

```xml
<policies>
  <inbound>
    <base />
    <rewrite-uri template="/openai/v1/chat/completions" />
  </inbound>
  <backend><base /></backend>
  <outbound><base /></outbound>
  <on-error><base /></on-error>
</policies>
```

`<base />` inherits the API-scope policies. The other operations need no policy of their own.

**Why `rewrite-uri` is at operation scope and not API scope:** at API scope all 40 operations
forward to chat completions, so `/test888/embeddings` and `/test888/batches` silently return chat
completions. Not a security hole — every operation still requires a valid JWT — but a correctness
trap for whoever pokes at the API next.

**Why `base-url` and not the import's `backend-id`:** using `base-url` bypasses the named
backend, which means the named backend's credentials are no longer applied — so the explicit
`authentication-managed-identity` line becomes **required, not optional**. The named backend
`test888-openai-endpoint` is left unused.

### 7. Point JASP at it

| Setting | Value |
|---|---|
| Endpoint | `https://jasp-apim-dev.azure-api.net/test888/chat/completions` |
| Auth mode | `oidc` |
| Authority | `organizations` (or pin to the tenant — see below) |
| Scope | `https://cognitiveservices.azure.com/.default` |
| Model | `gpt-5.4-mini` |

The endpoint must be the **operation's frontend path**, not the Azure-native one — the rewrite
handles the translation to the backend.

> **`endpoint` is the complete URL.** `aiBridge.cpp` uses it verbatim (`QNetworkRequest
> request{QUrl(ep)}`); nothing is appended. Configured as
> `https://test888.openai.azure.com`, it produced a POST to
> `…/openai/v1/chat/completions` — confirmed in the log, so the whole path lives in this field.

**On authority:** the preset ships `organizations`, which accepts any work account. The policy
pins `tenant-id`, so an account from another tenant is rejected at APIM. Pinning `authAuthority`
to the tenant is the honest configuration and what a customer would do.

### 8. Result

```
BrowserTokenProvider: token acquired (aud=https://cognitiveservices.azure.com
                                       scp=user_impersonation oid=… tid=1ef0aea6-…)
AiBridge: POST #1 to https://jasp-apim-dev.azure-api.net/test888/chat/completions
```

Model reply returned. `oid` and `tid` being present in that line is what makes the identity-keyed
quota possible — we added them to the log for exactly this reason.

---

## Diagnostics that actually worked

### Which 404 is it? Read the body shape.

| Body | Author | Meaning |
|---|---|---|
| `{"statusCode":404,"message":"Resource not found"}` | **APIM** | Routing miss — no API/operation matched the path |
| `{"error":{"code":"404","message":"Resource not found"}}` | **Azure OpenAI** | APIM routed fine; the *backend* path is wrong |

That single distinction resolved several hours of ambiguity and should go in the customer guide.
A bare `"code": "404"` (rather than `"DeploymentNotFound"`) means the **path** is wrong, not the
deployment name.

### Which failure is it, in order

| Symptom | Cause |
|---|---|
| `Protocol "" is unknown`, `http=0` | **Client-side.** Malformed endpoint — almost always invisible whitespace from pasting out of the Azure portal. The log shows it as an extra space: `POST #1 to  https://…` |
| APIM-shaped 404 | The API suffix or operation path doesn't match. Check the **operation list** and the **URL suffix**. |
| `401 Access denied due to missing subscription key` | Step 5 not done |
| `401 Unauthorized` (our message) | `validate-azure-ad-token` rejected it — audience, or `client-application-ids` doesn't match the token's `appid` |
| `401 PermissionDenied: Principal does not have access to API/Operation` | MI role not propagated, or missing |
| Backend-shaped 404 | Path translation — see step 6 |
| **429** `Token limit will exceed based on estimated request tokens` | `llm-token-limit` working. See below. |

### The 429 is a feature, and it fires easily

With `tokens-per-minute="10"`, every request is refused — that was the deliberate test proving the
limiter is live. But the default of `1000` from the import is **also** too low to use:

| | Tokens |
|---|---|
| JASP system prompt (common + persona), **every request** | ~851 |
| One request with the intro message | ~908 |

One request consumes almost the entire 1000/minute budget, so the second — the intro plus the first
user message — is refused with `Try again in 60 seconds`. Budgets must be set with knowledge of
the client's baseline prompt size, not picked out of the air.

---

## Landmines found

**`authentication-managed-identity` overwrites the `Authorization` header.** It must run
**after** `llm-token-limit`, not before. If it runs first, the limiter parses *APIM's* token,
`oid` is the managed identity's object ID, and **every user silently shares one budget**. This is
an ordering bug that produces no error.

> Optional hardening: add `output-token-variable-name="userJwt"` to `validate-azure-ad-token` and
> key the limiter on `@(((Jwt)context.Variables[&quot;userJwt&quot;]).Claims.GetValueOrDefault(&quot;oid&quot;,&quot;anonymous&quot;))`.
> That makes the counter independent of policy order entirely.

**The operation JASP targets must exist.** After the import's 40 operations were trimmed, the
chat-completions operation was deleted along with the retirements, and every request 404'd at
APIM. Worth checking first when a routing 404 appears out of nowhere.

**Most of the import's 40 operations are dead surface.** They include assistants, threads, vector
stores, batches and evaluations — the Assistants API family, which OpenAI sunset on
**2026-08-26**. Don't try to make the whole import coherent; one operation is all we need.

**`rewrite-uri` scope matters.** See step 6.

---

## What is not proven yet

| Claim | Status |
|---|---|
| Shape 2 — gateway, same audience, user token, no second credential | ✅ **verified end-to-end** |
| `llm-token-limit` enforces a token budget | ✅ **verified** — the 429 above |
| `validate-azure-ad-token` accepts the user's token | ✅ **verified** — the request reached the backend through it |
| **The quota buckets are per-user** | ⚠️ **unproven.** The 429 proves the limiter runs, not that it is keyed on `oid` rather than silently falling back to `"anonymous"`. Both look identical with one account. **Test:** with two signed-in users, hammer the gateway as A and confirm B is unaffected. |
| Negative cases — a foreign tenant or a different calling app actually gets 401 | ⚠️ unproven |
| `output-token-variable-name` is accepted by `validate-azure-ad-token` | ⚠️ unproven |
| Shape 3 — gateway with its own scope | ⚠️ unproven |

---

## What a customer must change

Everything else in this document is copy-paste; these are the values that are ours:

| Field | Ours | Theirs |
|---|---|---|
| `tenant-id` (policy) | `1ef0aea6-…` | **their tenant** — read it from the `tid=` claim in JASP's own log line |
| `client-application-ids` | `fc57bc92-…` | JASP's client ID (ours, unless they register their own app) |
| `base-url` (backend) | `https://test888.openai.azure.com` | their resource's endpoint |
| `audiences` | `https://cognitiveservices.azure.com` | same, unless they use a Foundry project endpoint (`https://ai.azure.com`) |
| `tokens-per-minute` | `40000` | their governance decision |

---

## Appendix A — the original build checklist

Kept because it is the shortest route to a *working* auth path and remains accurate for the
parts it covers: Developer tier, system-assigned identity, the `Cognitive Services OpenAI User`
role, disabling the subscription requirement, and `validate-azure-ad-token` with
`audiences` + `client-application-ids`.

Superseded by this document where they disagree:

- "Skip the Language models import, define manually" — the import was used instead, which is why
  `rewrite-uri` exists. Both work; hand-defining avoids the rewrite.
- The generated `counter-key="@(context.Subscription.Id)"` must not survive — see §4 and step 6.
- The inline `authentication-managed-identity` line is **not** redundant when `base-url` replaces
  `backend-id`.
