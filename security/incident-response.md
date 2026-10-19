# Incident response

How we handle security incidents for JASP and its modules. Kept deliberately short: in an incident you should not be reading a manual, you should be doing step 1.

## Contact point

- Security reports arrive via the route in [SECURITY.md](../SECURITY.md) (email: security@jasp-stats.org; contact: Joris Goosen, CTO).
- Reports are acknowledged within **5 working days**.

## Triage (within 5 working days)

1. Is user data affected? JASP stores no user data on our servers, so this is rare — but check (e.g., telemetry, if ever introduced).
2. Is the integrity of distributed software affected (installer, module, update mechanism)? If yes → severity High, go to step "Contain".
3. Otherwise classify: Low (cosmetic/DoS of website) / Medium (exploitable but hard) / High (exploitable, user-facing).

## Contain

| Scenario | Action |
|---|---|
| Compromised signing key | Revoke immediately, rotate, disclose |
| Malicious module or dependency | Pull module from `remote-bundles.json`, publish advisory, fix |
| Compromised maintainer account | Revoke access/tokens, force credential reset, audit recent commits and releases |
| Tampered download | Take file offline, restore from CI, announce |
| Vulnerability in bundled library | Patch, ship fixed release, note in release notes |

## Disclose

- Public disclosure via a GitHub security advisory (and release notes for fixed versions). Credit reporters unless they ask not to.
- CVE requested for user-exploitable vulnerabilities in JASP or modules.
- If personal data turns out to be affected, the ISMS owner ensures GDPR obligations (72-hour authority notification where applicable) are met.

## Learn (the "Act" in PDCA)

Every High-severity incident and any incident whose root cause was a known-but-unmitigated risk ends with:

1. A postmortem note appended to the relevant risk-register entry (what happened, what we changed).
2. Register review date moved up if needed.

## Roles

- **Incident lead / ISMS owner:** Joris Goosen (CTO). Single decision maker for containment and disclosure.
- **Technical response:** core programmers, assigned per incident by the incident lead.
- **Communication:** incident lead (website, GitHub, mailing lists).
