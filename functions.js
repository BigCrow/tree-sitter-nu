/**
 * @param {string} field_name
 * @param {string} entry
 * @param {string} separator
 */
function general_body_rules(field_name, entry, separator) {
  return (/** @type {{ [x: string]: RuleOrLiteral; }} */ $) =>
    prec(
      20,
      seq(
        repeat(seq(field(field_name, $[entry]), $[separator])), // Normal entries MUST have a separator
        seq(field(field_name, $[entry]), optional($[separator])),
      ),
    );
}
exports.general_body_rules = general_body_rules;

/**
 * @param {string} suffix
 * @param {{ (_$: any): string; (_$: any): ChoiceRule; (arg0: any): RuleOrLiteral; }} terminator
 */
function parenthesized_body_rules(suffix, terminator) {
  const parenthesized = "_parenthesized";
  return {
    ..._block_body_rules(`${parenthesized}${suffix}`),

    /// pipeline
    [`pipeline${parenthesized}${suffix}`]: (
      /** @type {{ pipe_element_parenthesized: RuleOrLiteral; pipe_element: string; pipe_element_parenthesized_last: RuleOrLiteral; }} */ $,
    ) =>
      prec.right(
        seq(
          repeat(alias($.pipe_element_parenthesized, $.pipe_element)),
          alias($.pipe_element_parenthesized_last, $.pipe_element),
          terminator($),
        ),
      ),
  };
}
exports.parenthesized_body_rules = parenthesized_body_rules;

/**
 * @param {string} suffix
 * @param {{ ($: { _terminator: any; }): any; ($: { _terminator: RuleOrLiteral; }): ChoiceRule; (arg0: any): RuleOrLiteral; }} terminator
 */
function block_body_rules(suffix, terminator) {
  return {
    ..._block_body_rules(suffix),

    /// pipeline
    [`pipeline${suffix}`]: (/** @type {any} */ $) =>
      prec.right(
        seq(
          repeat($.pipe_element),
          alias($.pipe_element_last, $.pipe_element),
          terminator($),
        ),
      ),
  };
}
exports.block_body_rules = block_body_rules;

/// To parse pipelines correctly grammar needs to know now pipeline may end.
/// For example in following closure
/// ```
/// {||
///   print qwe
///   print rty
/// }
/// ```
/// two print calls must be separated either by newline or ';', but last call
/// may not be separated from closing bracket at all `{|| print qwe; print rty}`
/// and in `()` blocks newlines are not considered statement terminators at all.
/// To correctly parse these situations distinct rules for different types of
/// statements are needed. These rules are differentiated by suffix, and only
/// difference between them is terminator parameter used in pipeline rule that
/// is terminating statements. This function automatically generates all rules
/// for a given terminator and names them with specified suffix.
/**
 * @param {string} suffix
 */
function _block_body_rules(suffix) {
  /**
   * @param {{ [x: string]: string; }} $
   * @param {string} rule_name
   * @param {string} suffix
   */
  function alias_for_suffix($, rule_name, suffix) {
    if (suffix == "") {
      return $[rule_name];
    } else {
      return alias($[rule_name + suffix], $[rule_name]);
    }
  }

  return {
    ["_block_body_statement" + suffix]: (
      /** @type {{ [x: string]: RuleOrLiteral; }} */ $,
    ) => choice($["_declaration" + suffix], $["_statement" + suffix]),

    /// Declarations
    ["_declaration" + suffix]: (
      /** @type {{ [x: string]: string; decl_def?: any; decl_export?: any; decl_extern?: any; decl_module?: any; decl_use?: any; }} */ $,
    ) =>
      choice(
        alias_for_suffix($, "decl_alias", suffix),
        $.decl_def,
        $.decl_export,
        $.decl_extern,
        $.decl_module,
        $.decl_use,
      ),

    ["decl_alias" + suffix]: (
      /** @type {{ [x: string]: string; _command_name?: any; }} */ $,
    ) =>
      seq(
        optional(MODIFIER().visibility),
        KEYWORD().alias,
        field("name", $._command_name),
        PUNC().eq,
        field("value", alias_for_suffix($, "pipeline", suffix)),
      ),

    /// Storage statements
    ["stmt_let" + suffix]: (/** @type {{ [x: string]: RuleOrLiteral; }} */ $) =>
      prec.right(
        1,
        seq(
          choice(KEYWORD().let, KEYWORD().let_env),
          $["_assignment_pattern" + suffix],
        ),
      ),

    ["stmt_mut" + suffix]: (/** @type {{ [x: string]: RuleOrLiteral; }} */ $) =>
      prec.right(1, seq(KEYWORD().mut, $["_assignment_pattern" + suffix])),

    ["stmt_const" + suffix]: (
      /** @type {{ [x: string]: RuleOrLiteral; }} */ $,
    ) =>
      prec.right(
        1,
        seq(
          optional(MODIFIER().visibility),
          KEYWORD().const,
          $["_assignment_pattern" + suffix],
        ),
      ),

    ["_assignment_pattern" + suffix]: (
      /** @type {{ [x: string]: string; _variable_name?: any; param_type?: any; }} */ $,
    ) =>
      seq(
        field("name", $._variable_name),
        field("type", optional($.param_type)),
        PUNC().eq,
        field("value", alias_for_suffix($, "pipeline", suffix)),
      ),

    /// Statements
    ["_statement" + suffix]: (
      /**⋅@type {any} */ /** @type {{ [x: string]: string; _ctrl_statement?: any; _stmt_hide?: any; _stmt_overlay?: any; stmt_register?: any; stmt_source?: any; assignment?: any; }} */ $,
    ) =>
      choice(
        $._ctrl_statement,
        $._stmt_hide,
        $._stmt_overlay,
        $.stmt_register,
        $.stmt_source,
        $.assignment,
        alias_for_suffix($, "stmt_let", suffix),
        alias_for_suffix($, "stmt_mut", suffix),
        alias_for_suffix($, "stmt_const", suffix),
        alias_for_suffix($, "pipeline", suffix),
      ),
  };
}
/**
 * @param {boolean} immediate
 */
function _decimal_rule(immediate) {
  const exponent = token.immediate(/[eE][-+]?[\d_]*\d[\d_]*/);
  const digits = token.immediate(/[\d_]*\d[\d_]*/);
  const head_token = immediate ? token.immediate : token;

  return (/** @type {any} */ _$) =>
    choice(
      seq(head_token(/[\d_]*\d[\d_]*/), optional(exponent)),
      seq(
        choice(head_token(OPR().minus), head_token(OPR().plus)),
        digits,
        optional(exponent),
      ),
      seq(
        head_token(/[\d_]*\d[\d_]*/),
        token.immediate(PUNC().dot),
        optional(digits),
        optional(exponent),
      ),
      seq(
        choice(head_token(OPR().minus), head_token(OPR().plus)),
        digits,
        token.immediate(PUNC().dot),
        optional(digits),
        optional(exponent),
      ),
      seq(head_token(PUNC().dot), digits, optional(exponent)),
      seq(
        choice(head_token(OPR().minus), head_token(OPR().plus)),
        optional(token.immediate(/_+/)),
        token.immediate(PUNC().dot),
        digits,
        optional(exponent),
      ),
    );
}
exports._decimal_rule = _decimal_rule;

/**
 * @param {boolean} in_list
 */
function _unquoted_rule(in_list) {
  const pattern = in_list
    ? /[^-$\s\n\t\r{}()\[\]"`';,][^\s\n\t\r{}()\[\]"`';,]*/
    : /[^-$\s\n\t\r{}()\[\]"`';][^\s\n\t\r{}()\[\]"`';]*/;
  const pattern_repeat = in_list
    ? /[^\s\n\t\r{}()\[\]"`';,]*/
    : /[^\s\n\t\r{}()\[\]"`';]*/;
  const pattern_repeat1 = in_list
    ? /[^\s\n\t\r{}()\[\]"`';,]+/
    : /[^\s\n\t\r{}()\[\]"`';]+/;
  const pattern_once = in_list
    ? /[^\s\n\t\r{}()\[\]"`';,]/
    : /[^\s\n\t\r{}()\[\]"`';]/;
  const pattern_with_dot = in_list
    ? /[^\s\n\t\r{}()\[\]"`';,.]/
    : /[^\s\n\t\r{}()\[\]"`';.]/;
  const pattern_with_le = in_list
    ? /[^\s\n\t\r{}()\[\]"`';,=<]/
    : /[^\s\n\t\r{}()\[\]"`';=<]/;
  const pattern_with_dollar = in_list
    ? /[^\s\n\t\r{}()\[\]"`';,$]/
    : /[^\s\n\t\r{}()\[\]"`';$]/;

  // because this catches almost anything, we want to ensure it is
  // picked as the a last resort after everything else has failed.
  // so we give it a ridiculously low precedence and place it at the
  // very end
  return (
    /** @type {{ _val_number_decimal: RuleOrLiteral; _immediate_decimal: RuleOrLiteral; }} */ $,
  ) =>
    prec.left(
      -69,
      choice(
        token(prec(-69, token(pattern))),

        // distinguish between unquoted and val_range in cmd_arg
        seq(
          token(PUNC().dot),
          token.immediate(pattern_with_dot),
          token.immediate(pattern_repeat1),
        ),
        seq(
          token(PUNC().dot),
          token.immediate(PUNC().dot),
          token.immediate(pattern_with_le),
          token.immediate(pattern_repeat),
        ),
        seq(
          token(PUNC().dot),
          token.immediate(PUNC().dot),
          token.immediate(PUNC().eq),
          token.immediate(pattern_with_dollar),
          token.immediate(pattern_repeat),
        ),
        seq(
          token(PUNC().dot),
          token.immediate(PUNC().dot),
          token.immediate(BRACK().open_angle),
          token.immediate(pattern_with_dollar),
          token.immediate(pattern_repeat),
        ),
        seq(
          token(PUNC().dot),
          token.immediate(PUNC().dot),
          token.immediate(PUNC().dot),
          token.immediate(pattern_repeat),
        ),
        seq(
          token(PUNC().dot),
          token.immediate(PUNC().dot),
          choice(
            token.immediate(PUNC().eq),
            token.immediate(BRACK().open_angle),
          ),
        ),
        seq(
          token(PUNC().dot),
          token.immediate(PUNC().dot),
          optional(token.immediate(pattern_once)),
        ),
        seq(token(PUNC().dot), optional(token.immediate(pattern_with_dot))),

        // distinguish between $.val_number and unquoted string starting with numeric characters
        seq(
          choice(
            $._val_number_decimal,
            token(SPECIAL().pos_infinity),
            token(SPECIAL().neg_infinity),
            token(SPECIAL().not_a_number),
          ),
          token.immediate(pattern_once),
          token.immediate(pattern_repeat),
        ),

        // recognize unquoted string starting with numeric characters
        // e.g. 192.168.0.1
        seq(
          $._val_number_decimal,
          token.immediate(PUNC().dot),
          $._immediate_decimal,
          token.immediate(PUNC().dot),
          $._immediate_decimal,
          token.immediate(pattern_repeat1),
        ),
      ),
    );
}
exports._unquoted_rule = _unquoted_rule;

/// nushell keywords
function KEYWORD() {
  return {
    def: "def",
    alias: "alias",
    use: "use",
    export_env: "export-env",
    extern: "extern",
    module: "module",

    let: "let",
    let_env: "let-env",
    mut: "mut",
    const: "const",

    hide: "hide",
    hide_env: "hide-env",

    source: "source",
    source_env: "source-env",

    overlay: "overlay",
    register: "register",

    for: "for",
    loop: "loop",
    while: "while",
    error: "error",

    do: "do",
    if: "if",
    else: "else",
    try: "try",
    catch: "catch",
    match: "match",

    break: "break",
    continue: "continue",
    return: "return",

    as: "as",
    in: "in",
  };
}
exports.KEYWORD = KEYWORD;

// modifier keywords
function MODIFIER() {
  return {
    overlay_hide: "hide",
    overlay_list: "list",
    overlay_new: "new",
    overlay_use: "use",

    error_make: "make",

    visibility: "export",
  };
}
exports.MODIFIER = MODIFIER;

// redirection
function REDIR() {
  return ["err>", "out>", "e>", "o>", "err+out>", "out+err>", "o+e>", "e+o>"];
}
exports.REDIR = REDIR;

// punctuation
function PUNC() {
  return {
    at: "@",
    dot: ".",
    hash: "#",
    pipe: "|",
    rest: "...",
    eq: "=",
    colon: ":",
    comma: ",",
    caret: "^",
    dollar: "$",
    fat_arrow: "=>",
    thin_arrow: "->",
    question: "?",
    underscore: "_",

    semicolon: ";",
  };
}
exports.PUNC = PUNC;

// delimiters
function BRACK() {
  return {
    open_angle: "<",
    close_angle: ">",

    open_brack: "[",
    close_brack: "]",

    open_brace: "{",
    close_brace: "}",

    open_paren: "(",
    close_paren: ")",
  };
}
exports.BRACK = BRACK;

// operators
function OPR() {
  return {
    // arithmetic
    plus: "+",
    minus: "-",
    times: "*",
    divide: "/",
    modulo: "mod",
    floor: "//",
    power: "**",
    append: "++",

    // comparison
    equal: "==",
    not_equal: "!=",
    less_than: "<",
    less_than_equal: "<=",
    greater_than: ">",
    greater_than_equal: ">=",

    // regex matching
    regex_match: "=~",
    regex_not_match: "!~",

    // logical
    not: "not",
    and: "and",
    or: "or",
    xor: "xor",

    // bitwise
    bit_or: "bit-or",
    bit_xor: "bit-xor",
    bit_and: "bit-and",
    bit_shl: "bit-shl",
    bit_shr: "bit-shr",

    // membership tests
    in: "in",
    not_in: "not-in",
    starts_with: "starts-with",
    ends_with: "ends-with",

    // assignment
    assign_add: "+=",
    assign_sub: "-=",
    assign_mul: "*=",
    assign_div: "/=",
    assign_append: "++=",

    // range
    range_inclusive: "..",
    range_inclusive2: "..=",
    range_exclusive: "..<",
  };
}
exports.OPR = OPR;

/// operator precedence
/// taken from `nu_protocol::`
function PREC() {
  return {
    range: 15,
    power: 14,
    multiplicative: 13,
    additive: 12,
    bit_shift: 11,
    comparative: 10,
    membership: 9,
    regex: 8,
    bit_and: 7,
    bit_xor: 6,
    bit_or: 5,
    and: 4,
    xor: 3,
    or: 2,
    assignment: 1,
  };
}
exports.PREC = PREC;

function STATEMENT_PREC() {
  return {
    control: 1,
  };
}
exports.STATEMENT_PREC = STATEMENT_PREC;

/// map of operators and their precedence
function TABLE() {
  const multiplicatives = choice(
    OPR().times,
    OPR().divide,
    OPR().modulo,
    OPR().floor,
  );

  const comparatives = choice(
    OPR().equal,
    OPR().not_equal,
    OPR().less_than,
    OPR().less_than_equal,
    OPR().greater_than,
    OPR().greater_than_equal,
  );

  const memberships = choice(
    OPR().in,
    OPR().not_in,
    OPR().starts_with,
    OPR().ends_with,
  );

  // `range` is not included here and is handled separately
  return [
    [PREC().power, choice(OPR().power, OPR().append)],
    [PREC().multiplicative, multiplicatives],
    [PREC().additive, choice(OPR().plus, OPR().minus)],
    [PREC().bit_shift, choice(OPR().bit_shl, OPR().bit_shr)],
    [PREC().comparative, comparatives],
    [PREC().membership, memberships],
    [PREC().regex, choice(OPR().regex_match, OPR().regex_not_match)],
    [PREC().bit_and, OPR().bit_and],
    [PREC().bit_xor, OPR().bit_xor],
    [PREC().bit_or, OPR().bit_or],
    [PREC().and, OPR().and],
    [PREC().xor, OPR().xor],
    [PREC().or, OPR().or],
  ];
}
exports.TABLE = TABLE;

/// special tokens
function SPECIAL() {
  return {
    true: "true",
    false: "false",
    null: "null",

    pos_infinity: /[iI][nN][fF]([iI][nN][iI][tT][yY])?/,
    neg_infinity: /-[iI][nN][fF]([iI][nN][iI][tT][yY])?/,
    not_a_number: /[nN][aA][nN]/,
  };
}
exports.SPECIAL = SPECIAL;

/// nushell flat types
/// taken from `nu_parser::parser::parse_shape_name()`
function FLAT_TYPES() {
  // prettier-ignore
  const types = [
        "any", "binary", "block", "bool", "cell-path", "closure", "cond",
        "datetime", "directory", "duration", "directory", "duration",
        "error", "expr", "float", "decimal", "filesize", "full-cell-path",
        "glob", "int", "import-pattern", "keyword", "math", "nothing",
        "number", "one-of", "operator", "path", "range", "signature",
        "string", "table", "variable", "var-with-opt-type", "record", "list",
    ];

  return choice(...types);
}
exports.FLAT_TYPES = FLAT_TYPES;
