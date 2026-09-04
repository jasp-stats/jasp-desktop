# Security policy

## Reporting a vulnerability

Please report security vulnerabilities in JASP or its modules privately — do **not** open a public issue.

- **GitHub:** use [private vulnerability reporting](https://docs.github.com/en/code-security/security-advisories/guidance-for-reporting-and-fixing-vulnerabilities/privately-reporting-a-security-vulnerability) on the affected repository (jasp-stats/jasp-desktop or the module repository).
- **Email:** [security@jasp-stats.org](mailto:security@jasp-stats.org)

We aim to acknowledge reports within 5 working days. Please include a description of the issue, steps to reproduce, and affected versions.

## Scope

- The JASP desktop application and its bundled analyses
- JASP modules (including R code they execute)
- The distribution pipeline: installers, download servers, update/module-bundle mechanism

Out of scope: website content defacement via compromised third-party accounts that do not affect software distribution, and social engineering of JASP team members.

## Safe harbor

We consider security research conducted in good faith, within this scope, and reported privately to be authorized activity. We will not pursue legal action against such research and will credit reporters in advisories unless asked otherwise.

## Supported versions

Security fixes are made for the latest stable release only. See [version.txt](version.txt) for the current version.

Our internal security management (ISMS) documentation is in [security/](security/).
