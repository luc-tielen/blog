/** TextMate grammar for Soufflé Datalog */
export default {
  name: 'datalog',
  scopeName: 'source.datalog',
  aliases: ['dl'],
  patterns: [
    { include: '#line_comment' },
    { include: '#string' },
    { include: '#directive' },
    { include: '#builtin_type' },
    { include: '#number' },
  ],
  repository: {
    line_comment: {
      match: '//.*$',
      name: 'comment.line.double-slash.datalog',
    },
    string: {
      begin: '"',
      end: '"',
      name: 'string.quoted.double.datalog',
    },
    directive: {
      match: '\\.(decl|input|output|type|comp|instantiate|override|init|functor|pragma)\\b',
      name: 'keyword.control.datalog',
    },
    builtin_type: {
      match: '\\b(number|unsigned|float|symbol|cat)\\b',
      name: 'storage.type.datalog',
    },
    number: {
      match: '\\b[0-9]+\\b',
      name: 'constant.numeric.datalog',
    },
  },
};
