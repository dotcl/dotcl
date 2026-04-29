# Contributing

Issues and pull requests are welcome.

## Reporting bugs

Please use the bug report template and include:

- Your OS and CPU architecture
- `.NET SDK` version (`dotnet --version`)
- dotcl version or commit hash
- A minimal Lisp form that reproduces the issue

## Submitting changes

1. Fork the repository and create a topic branch from `master`.
2. Make focused changes.
3. Run the test suites:
   ```bash
   make cross-compile
   make test-regression       # ~5s, dotcl-specific tests
   make test-ansi-full        # ~3min, ANSI conformance regression
   ```
4. Open a pull request against `master` using the PR template.

## License

By contributing, you agree that your contributions will be licensed under
the [MIT License](LICENSE).

## Code of Conduct

Participation is governed by the [Code of Conduct](CODE_OF_CONDUCT.md).
