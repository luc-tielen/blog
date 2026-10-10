/** TextMate grammar for GDB scripts */
export default {
  name: 'gdb',
  scopeName: 'source.gdb',
  patterns: [
    { include: '#comment' },
    { include: '#variable' },
    { include: '#command' },
    { include: '#number' },
  ],
  repository: {
    comment: {
      match: '#.*$',
      name: 'comment.line.number-sign.gdb',
    },
    variable: {
      match: '\\$[a-zA-Z_][a-zA-Z0-9_]*',
      name: 'variable.other.gdb',
    },
    command: {
      // prettier-ignore
      match:
        '\\b(break|b|run|r|continue|c|next|n|step|s|finish|print|p|info|set|show|' +
        'backtrace|bt|command|commands|end|delete|disable|enable|quit|q|watch|list|l|' +
        'up|down|frame|f|return|call|define|document|hook|while|if|else|' +
        'loop_break|loop_continue|tbreak|hbreak|rbreak|clear|condition|' +
        'ignore|handle|signal|catch|throw|exec|attach|detach|kill|load|' +
        'add-symbol-file|file|core|target|source|directory|path|thread|' +
        'maintenance|maint|monitor|checkpoint|restart|record|reverse|x|' +
        'display|undisplay|printf|output|echo|help|apropos|alias)\\b',
      name: 'keyword.control.gdb',
    },
    number: {
      match: '\\b[0-9]+\\b',
      name: 'constant.numeric.gdb',
    },
  },
};
