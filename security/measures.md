# Existing measures

Inventory of controls already in place, per NCSC ("breng in kaart wat er al is, voordat je nieuwe plannen maakt"). This file is also the evidence base for external parties asking about our security posture.

## Access control

- Two-factor authentication enforced on the jasp-stats GitHub organization.
- Branch protection on release-relevant branches; pull requests require review by at least one JASP member (see CONTRIBUTING.md).
- Core programmers may override branch protection — **deliberate, accepted trade-off** recorded as risk R07/R15 in the risk register.
- Minimal number of organization admins.

## Software supply chain

- CI and test builds for all three platforms run in GitHub Actions from versioned workflows; release artifacts are built and signed per platform:
  - **Windows:** Microsoft Store builds are signed by Microsoft; MSIs are signed with our own key (previously the University of Amsterdam's); zip builds are unsigned but a SHA-256 checksum is published.
  - **macOS:** DMGs are signed with an Apple-issued key and notarized by Apple.
  - **Linux:** Flathub builds and signs the Flatpak package on their own infrastructure; we contribute updates via pull requests to Flathub.
- R package dependencies pinned via renv lockfiles (`Modules/install-renv.R.in`), so releases are reproducible.
- The JASP engine runs sandboxed on Windows, limiting what malicious R code (e.g., from a trojaned dependency) could reach. On Linux, the Flatpak sandbox provides partial isolation, but retains access to the user's home directory.
- Module set is curated in `Modules/modules-settings.json`; remote module bundles served from controlled infrastructure over HTTPS (`Modules/remote-bundles.json`).
- Code review requirement for all PRs, including new modules and analyses.

## Distribution

- Distributed artifacts carry platform-appropriate integrity protection: OS-verified signatures (Microsoft Store, MSI, DMG, Flatpak) and published SHA-256 checksums for unsigned zip builds.
- Website and downloads served over HTTPS via external hosting.

## Process and people

- Public vulnerability reporting route via [SECURITY.md](../SECURITY.md).
- Incident triage and disclosure process in [incident-response.md](incident-response.md).
- Development documentation lives in `Docs/development`, reducing bus factor.
- Security decisions are recorded in the [risk register](risk-register.md) with an owner, including consciously accepted risks — no silent gaps.

## Gaps (feed into next PDCA cycle)

- Periodic audit of GitHub Actions workflow permissions and secrets (R02).
- Automated dependency-update alerting for R packages and bundled system libraries (R04, R08).
- Documented hosting-provider responsibilities for website and downloads (R05, R09).
