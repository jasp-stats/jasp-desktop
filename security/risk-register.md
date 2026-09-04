# Risk register

Scale: Likelihood and Impact are Low / Medium / High. Level = L × I.

| ID | Risk | Asset | Likelihood | Impact | Level | Mitigation | Owner | Status |
|---|---|---|---|---|---|---|---|---|
| R01 | Code-signing key stolen or misused | Signing keys | Low | High | **High** | Per-platform keys: Microsoft Store signing (managed by Microsoft), Apple code-signing key + notarization, own MSI signing key (we switched to our own keys; the earlier University of Amsterdam key remains valid and uncompromised, so UvA-signed MSIs in circulation are authentic). TODO: document key storage for the Apple and MSI keys; incident-response.md covers revocation | Joris Goosen | Partial |
| R02 | CI secret leaked via workflow or log | CI secrets | Medium | High | **High** | Least-privilege tokens; secrets not echoed to logs; release signing keys are kept on our own build machines (Mac minis), not in CI. A dead macOS workflow that referenced a code-signing certificate in GitHub secrets was removed (2026-09). TODO: periodic audit of workflow permissions and secrets | Joris Goosen | Partial |
| R03 | Compromised core maintainer GitHub account | Maintainer accounts | Medium | High | **High** | 2FA enforced; branch protection with core-dev override; TODO: passkeys/hardware keys for admins | Joris Goosen | Partial |
| R04 | Malicious or trojaned R dependency in a module | Module repos, dependencies | Medium | High | **High** | Dependencies pinned via renv lockfile; impact of malicious R code is limited by engine sandboxing on Windows and by the Flatpak sandbox on Linux (partial: home directory remains accessible). TODO: periodic dependency review + automated alerting (Dependabot/Renovate) | Joris Goosen | Partial |
| R05 | Tampered installer on download server / CDN | Download servers | Low | High | **High** | Signed installers (OS-level verification); HTTPS; TODO: document hosting provider's controls | Joris Goosen | Open |
| R06 | GitHub organization admin account takeover | GitHub org | Low | High | **High** | 2FA enforced; minimal number of admins; see R03 | Joris Goosen | Partial |
| R07 | Malicious PR merged to a release branch | Source code | Low | High | **High** | PRs require review by a JASP member; branch protection; branch-protection override by core devs is an accepted, deliberate risk | Joris Goosen | Done |
| R08 | Vulnerability in a third-party system library shipped with JASP | Dependencies | High | Medium | **High** | TODO: track CVEs for bundled libs; update in next release cycle | Joris Goosen | Open |
| R09 | Website defacement or DNS hijack of jasp-stats.org | Website | Low | Medium | Medium | Hosted by external provider; TODO: record provider and its responsibilities | Joris Goosen | Open |
| R10 | Update/upgrade mechanism serves tampered bundle config | remote-bundles.json | Low | High | **High** | Served over HTTPS from controlled infrastructure; TODO: consider signing bundle manifests | Joris Goosen | Partial |
| R11 | Abandoned module with unpatched vulnerability | Module repos | Medium | Medium | Medium | Module list in modules-settings.json is curated; security contact exists (SECURITY.md) | Joris Goosen | Done |
| R12 | Loss of sole-maintainer knowledge (bus factor) | All | Medium | Medium | Medium | Development docs in Docs/; ISMS itself is documented; at least two people are able to cut a release | Joris Goosen | Done |
| R13 | Accidental deletion/corruption of source or release artifacts | Source code, artifacts | Low | Medium | Medium | Git remotes and distributed clones; artifacts reproducible from source via CI | Joris Goosen | Done |
| R14 | Security report (vulnerability disclosure) mishandled → reputational damage | Reputation | Medium | Medium | Medium | SECURITY.md with clear reporting route; incident-response.md defines triage and disclosure | Joris Goosen | Done |
| R15 | Insider misuse of broad access by a core programmer | Source code, releases | Low | Medium | Medium | Small trusted team; full git history is a deterrent and audit trail; PR review culture | Joris Goosen | Done |
| R16 | Supply-chain compromise of the R ecosystem itself (CRAN mirror, etc.) | Dependencies | Low | High | **High** | Outside our direct control; rely on CRAN's own checks; pin versions via renv; impact limited by engine sandboxing (Windows) and Flatpak sandbox (Linux, partial); monitor advisories | Joris Goosen | Open |

## Rules for this register

- Every risk has exactly one owner; currently that is the ISMS owner (Joris Goosen). Delegation is fine, accountability is not.
- New risks are added the moment they are identified — even mid-cycle — so nothing is forgotten (NCSC advice).
- Status values: `Open`, `Partial`, `Done`, `Accepted` (accepted risk = consciously chosen not to mitigate, with a one-line rationale).
- Reviewed at every major release and at least annually; review is logged in [README.md](README.md).
