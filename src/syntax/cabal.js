/** TextMate grammar for Haskell Cabal files */
export default {
  name: 'cabal',
  scopeName: 'source.cabal',
  patterns: [
    { include: '#comment' },
    { include: '#section' },
    { include: '#field_name' },
    { include: '#string' },
    { include: '#version' },
  ],
  repository: {
    comment: {
      match: '--.*$',
      name: 'comment.line.double-dash.cabal',
    },
    section: {
      match:
        '^(library|executable|test-suite|benchmark|flag|source-repository|custom-setup|common)(?:\\s+\\S+)?',
      captures: {
        1: { name: 'keyword.control.cabal' },
      },
    },
    field_name: {
      match:
        '^[ \\t]+(name|version|synopsis|description|homepage|bug-reports|license|' +
        'license-file|author|maintainer|copyright|category|build-type|cabal-version|' +
        'build-depends|exposed-modules|other-modules|hs-source-dirs|main-is|' +
        'ghc-options|ghc-prof-options|ghcjs-options|extra-source-files|data-files|' +
        'data-dir|include-dirs|includes|install-includes|c-sources|js-sources|' +
        'cc-options|ld-options|pkgconfig-depends|frameworks|extra-libraries|' +
        'extra-lib-dirs|default-language|default-extensions|other-extensions|' +
        'type|test-module|benchmark-module|mixins|virtual-modules|autogen-modules|' +
        'autogen-includes|cxx-sources|cxx-options|asm-sources|asm-options|' +
        'scope|import|reexported-modules|signatures)(?=\\s*:)',
      name: 'entity.name.function.cabal',
    },
    string: {
      begin: '"',
      end: '"',
      name: 'string.quoted.double.cabal',
    },
    version: {
      match: '\\b[0-9]+(?:\\.[0-9]+)+\\b',
      name: 'constant.numeric.cabal',
    },
  },
};
