# JASP Information Security Management System (ISMS)

**Owner and sponsor:** Joris Goosen (CTO)
**Scope:** JASP desktop application and its modules
**Cycle:** reviewed at every major release and at least annually (PDCA)

## What this ISMS is

This ISMS is deliberately minimal. It follows the [NCSC](https://www.ncsc.nl/risicomanagement/beginnen-met-een-isms) approach: start small, protect what matters most, and improve continuously. It consists of five short documents that together answer:

1. What must we protect? — [assets.md](assets.md)
2. What could go wrong? — [risk-register.md](risk-register.md)
3. What do we already do about it? — [measures.md](measures.md)
4. What do we do when something happens? — [incident-response.md](incident-response.md)

Everything is plain Markdown in git; the git history serves as the audit trail. There is no separate tooling, no certification ambition, and no document-approval ceremony.

## Scope for cycle 1

| In scope | Out of scope (revisit next cycle) |
|---|---|
| Software supply chain: source code, CI/CD, signing keys, module repositories | Office IT / physical security |
| Distribution infrastructure: jasp-stats.org, download servers, installers/updater | Personal devices of contributors (BYOD) |
| Maintainer account access: GitHub org, permissions, 2FA | HR/administrative processes |

## How to report a security issue

See [SECURITY.md](../SECURITY.md) in the repository root.

## Review log

| Date | Reviewed by | Changes |
|---|---|---|
| 2026-09-04 | Joris Goosen | Initial version |
