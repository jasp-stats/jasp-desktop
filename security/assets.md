# Assets — what must we protect

Ratings are per the CIA triad (Confidentiality, Integrity, Availability). For JASP, **integrity dominates**: researchers must be able to trust that a downloaded JASP binary or module does exactly what the source code says.

## Asset group A — Software supply chain

| Asset | C | I | A | Notes |
|---|---|---|---|---|
| Source code (jasp-desktop, module repos) | – | High | Medium | The single source of truth for what JASP does |
| Code-signing keys (per platform: Apple key + notarization, own MSI key; Microsoft Store signing is managed by Microsoft; Flathub signs Linux builds) | High | High | High | Compromise = signed malware distributed under our name. Older MSIs were signed by the University of Amsterdam key, which remains valid and uncompromised |
| CI/CD secrets and tokens (GitHub Actions) | High | High | Medium | Used in release builds for all three OSes |
| R module repositories | – | High | Medium | Modules run R code inside the user's session |
| Third-party dependencies (R packages, system libs) | – | High | Medium | We inherit their vulnerabilities |

## Asset group B — Distribution infrastructure

| Asset | C | I | A | Notes |
|---|---|---|---|---|
| jasp-stats.org (website, documentation) | – | High | High | Public face; defacement damages trust |
| Download servers / CDN | – | High | High | Primary delivery path of installers |
| Installers / update mechanism | – | High | Medium | Tampering here reaches end users directly |
| Module bundle configuration (`remote-bundles.json`, `modules-settings.json`) | – | High | Medium | Decides which modules users install |

## Asset group C — Maintainer accounts

| Asset | C | I | A | Notes |
|---|---|---|---|---|
| GitHub organization (jasp-stats) | – | High | High | Controls all source and releases |
| Core maintainer accounts | High | High | High | Account takeover ≈ full supply-chain compromise |
| Module author access | – | Medium | – | Writes code that ends up in user sessions |

## Crown jewels (the short version)

1. The **integrity of what we distribute** — a tampered installer or module is the worst-case scenario for JASP's reputation and for our users' research.
2. The **signing keys and CI secrets** that make that integrity demonstrable.
3. The **GitHub organization** that ties it all together.
