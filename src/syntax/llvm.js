/** TextMate grammar for LLVM IR (.ll files) */
export default {
  name: 'llvm',
  scopeName: 'source.llvm',
  aliases: ['ll', 'llvm-ir'],
  patterns: [
    { include: '#comment' },
    { include: '#string' },
    { include: '#type' },
    { include: '#keyword' },
    { include: '#global_var' },
    { include: '#local_var' },
    { include: '#number' },
  ],
  repository: {
    comment: {
      match: ';.*$',
      name: 'comment.line.semicolon.llvm',
    },
    string: {
      begin: '"',
      end: '"',
      name: 'string.quoted.double.llvm',
    },
    type: {
      match: '\\b(void|half|bfloat|float|double|fp128|x86_fp80|ppc_fp128|label|metadata|token|i[1-9][0-9]*)\\b',
      name: 'storage.type.llvm',
    },
    keyword: {
      // prettier-ignore
      match:
        '\\b(define|declare|global|constant|type|module|target|datalayout|triple|' +
        'attributes|metadata|distinct|external|internal|private|linkonce|linkonce_odr|' +
        'weak|weak_odr|appending|dllimport|dllexport|common|available_externally|' +
        'ccc|fastcc|coldcc|webkit_jscc|anyregcc|preserve_mostcc|preserve_allcc|' +
        'ret|br|switch|indirectbr|invoke|resume|unreachable|callbr|' +
        'add|fadd|sub|fsub|mul|fmul|udiv|sdiv|fdiv|urem|srem|frem|' +
        'and|or|xor|shl|lshr|ashr|' +
        'alloca|load|store|fence|cmpxchg|atomicrmw|getelementptr|' +
        'trunc|zext|sext|fptrunc|fpext|fptoui|fptosi|uitofp|sitofp|' +
        'ptrtoint|inttoptr|bitcast|addrspacecast|' +
        'icmp|fcmp|phi|select|call|va_arg|landingpad|catchpad|cleanuppad|' +
        'eq|ne|ugt|uge|ult|ule|sgt|sge|slt|sle|' +
        'oeq|ogt|oge|olt|ole|one|ord|ueq|une|uno|' +
        'true|false|null|zeroinitializer|undef|poison|' +
        'nuw|nsw|exact|volatile|inbounds|align|' +
        'inalloca|sret|noalias|nocapture|nest|returned|nonnull|' +
        'signext|zeroext|inreg|byval|byref|' +
        'nounwind|noreturn|readnone|readonly|writeonly|optnone|optsize|' +
        'alwaysinline|noinline|naked|builtin|cold|noduplicate|' +
        'speculatable|strictfp|uwtable|sanitize_address|sanitize_memory|sanitize_thread|' +
        'acquire|release|acq_rel|seq_cst|monotonic|unordered|syncscope|' +
        'x|to)\\b',
      name: 'keyword.control.llvm',
    },
    global_var: {
      match: '@[a-zA-Z_$\\.][a-zA-Z0-9_$\\.]*|@"[^"]*"|@[0-9]+',
      name: 'entity.name.function.llvm',
    },
    local_var: {
      match: '%[a-zA-Z_$\\.][a-zA-Z0-9_$\\.]*|%[0-9]+',
      name: 'variable.other.llvm',
    },
    number: {
      match: '\\b(?:0x[0-9a-fA-F]+|[0-9]+(?:\\.[0-9]+)?)\\b',
      name: 'constant.numeric.llvm',
    },
  },
};
