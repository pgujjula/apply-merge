<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# Changelog for apply-merge

## 0.1.1.0

### Release History

| Release         | Date       | Tag              |
| --------------- | ---------- | ---------------- |
| Initial Release | 2024-05-11 | [`0.1.1.0`]      |
| Revision 1      | 2024-05-13 | [`0.1.1.0-rev1`] |
| Revision 2      | 2024-06-25 | [`0.1.1.0-rev2`] |
| Revision 3      | 2024-12-11 | [`0.1.1.0-rev3`] |
| Revision 4      | 2025-01-07 | [`0.1.1.0-rev4`] |

### Added
* Implementation of `applyMergeOn`
  ([#1](https://github.com/pgujjula/apply-merge/issues/1)) and `applyMergeBy`
  ([#3](https://github.com/pgujjula/apply-merge/issues/3)).
* Implementation of `applyMerge` for `NonEmpty`
  ([#2](https://github.com/pgujjula/apply-merge/issues/2)).
* Comprehensive benchmark suite for `applyMerge`
  ([#8](https://github.com/pgujjula/apply-merge/issues/8)).
* Support for GHC 9.10 (beginning in Revision 1)

### Changed
* Switched from falsify to QuickCheck in the test suite, allowing the
  project to be used in Stackage
  ([link](https://github.com/well-typed/falsify/issues/71)).

## 0.1.0.0

## Release History

| Release             | Date       | Tag              |
| ------------------- | ---------- | ---------------- |
| Release Candidate 1 | 2024-04-05 | [`0.1.0.0-rc1`]  |
| Release Candidate 2 | 2024-04-10 | [`0.1.0.0-rc2`]  |
| Initial Release     | 2024-04-12 | [`0.1.0.0`]      |

### Added
* Implementation of `applyMerge`.
* A tasty-based test suite for `applyMerge`.
* A tasty-bench benchmark suite for `applyMerge`.
* Haddock documentation for `applyMerge`.
* Support for GHC 9.2, 9.4, 9.6, 9.8.
* Documentation of the library in `README.md` and `docs/ALGORITHM.md`, with
  feedback from _meeeow_ on Haskell Discourse
  ([link](https://discourse.haskell.org/t/apply-merge-lift-a-binary-increasing-function-onto-ordered-lists-and-produce-ordered-output/9269/4)).

[`0.1.1.0-rev4`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.1.0-rev4
[`0.1.1.0-rev3`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.1.0-rev3
[`0.1.1.0-rev2`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.1.0-rev2
[`0.1.1.0-rev1`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.1.0-rev1
[`0.1.1.0`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.1.0
[`0.1.0.0`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.0.0
[`0.1.0.0-rc2`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.0.0-rc2
[`0.1.0.0-rc1`]: https://github.com/pgujjula/apply-merge/releases/tag/0.1.0.0-rc1
