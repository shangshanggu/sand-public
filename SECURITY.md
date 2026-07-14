# Security Policy

## Scope

Security reports may concern exposed credentials, participant information, unsafe release files, dependency vulnerabilities, or code that could disclose protected data.

## Reporting

Do not open a public issue containing secrets, participant information, internal addresses, or exploit details. Use GitHub private vulnerability reporting when it is available. Otherwise, contact the repository owner privately through the contact method on their GitHub profile and share only enough information to establish a secure channel.

## Credential Handling

Local credentials belong in ignored files such as `.env` or `.mcp.json`. Never commit them. Anyone who discovers a committed credential should revoke or rotate it; deleting the file from the latest commit does not remove it from Git history.

## Participant Data

Real SAND exports must remain outside Git under `reproduced/data/raw/`. Do not attach them to issues, CI artifacts, releases, or debugging logs.

