#include "tree_sitter/alloc.h"
#include "tree_sitter/parser.h"
#include <stdint.h>

// Based on tree-sitter/tree-sitter-rust imlpementation for raw_string_literals.

// Tree-sitter objects

enum TokenType {
    COMMENT,
    MATH_KEYWORD,
    MATH_SIGN,
    RAW_STRING_LITERAL_START,
    RAW_STRING_LITERAL_CONTENT,
    RAW_STRING_LITERAL_END,
    ERROR_SENTINEL,
};

typedef struct {
    uint8_t opening_hash_count;
} Scanner;

void *tree_sitter_nu_external_scanner_create() {
    return ts_calloc(1, sizeof(Scanner));
}

void tree_sitter_nu_external_scanner_destroy(void *payload) {
    ts_free((Scanner *)payload);
}

unsigned tree_sitter_nu_external_scanner_serialize(void *payload,
                                                   char *buffer) {
    Scanner *scanner = (Scanner *)payload;
    buffer[0] = (char)scanner->opening_hash_count;
    return 1;
}

void tree_sitter_nu_external_scanner_deserialize(void *payload,
                                                 const char *buffer,
                                                 unsigned length) {
    Scanner *scanner = (Scanner *)payload;
    scanner->opening_hash_count = 0;
    if (length == 1) {
        Scanner *scanner = (Scanner *)payload;
        scanner->opening_hash_count = buffer[0];
    }
}

// Utility functions
// Lexer movement

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

// Regex functions
// used for case insensitivity
static inline int32_t upper_case(int32_t letter) {
    if (letter > 96 || letter < 123)
        return letter - 32;
    return letter;
};

static inline bool test_word(TSLexer *lexer, const char *word,
                             bool case_sensitivity) {
    int length = 0;
    while (word[length] != '\0') {
        length++;
    }
    if (!case_sensitivity) {
        for (int i = 0; i < length; ++i) {
            if (upper_case(lexer->lookahead) != upper_case(word[i]))
                return false;
            advance(lexer);
        };
    } else {
        for (int i = 0; i < length; ++i) {
            if (lexer->lookahead != word[i])
                return false;
            advance(lexer);
        };
    };
    return true;
}

static inline bool test_char_range(int32_t chr, const char *range) {
    int length = 0;
    while (range[length] != '\0') {
        length++;
    }

    bool res = false;
    for (int i = 0; i < length; ++i) {
        if (chr == range[i]) {
            res = true;
            break;
        }
    };
    return res;
}

static inline bool common_terminator(TSLexer *lexer) {
    return test_char_range(lexer->lookahead, ",; \n");
}

// Raw string functions

const char RAW_STRING_QUOTE_CHAR = 39; // '

static inline bool scan_raw_string_start(Scanner *scanner, TSLexer *lexer) {
    advance(lexer);

    uint8_t opening_hash_count = 0;
    while (lexer->lookahead == '#') {
        advance(lexer);
        ++opening_hash_count;
    }

    // Different from rust - requires at least 1 #
    if (opening_hash_count == 0) {
        return false;
    }

    if (lexer->lookahead != RAW_STRING_QUOTE_CHAR) {
        return false;
    }
    advance(lexer);

    scanner->opening_hash_count = opening_hash_count;
    lexer->result_symbol = RAW_STRING_LITERAL_START;
    return true;
}

static inline bool scan_raw_string_content(Scanner *scanner, TSLexer *lexer) {
    while (true) {
        if (lexer->eof(lexer)) {
            return false;
        }
        if (lexer->lookahead == RAW_STRING_QUOTE_CHAR) {
            lexer->mark_end(lexer);
            advance(lexer);
            unsigned hash_count = 0;
            while (lexer->lookahead == '#' &&
                   hash_count < scanner->opening_hash_count) {
                advance(lexer);
                ++hash_count;
            }
            if (hash_count == scanner->opening_hash_count) {
                lexer->result_symbol = RAW_STRING_LITERAL_CONTENT;
                return true;
            }
        } else {
            advance(lexer);
        }
    }
}

static inline bool scan_raw_string_end(Scanner *scanner, TSLexer *lexer) {
    advance(lexer);
    for (unsigned i = 0; i < scanner->opening_hash_count; ++i) {
        advance(lexer);
    }

    // Nushell in contrast to rust do not allow for a comment right after a raw
    // string.
    if (lexer->lookahead == '#')
        return false;

    lexer->result_symbol = RAW_STRING_LITERAL_END;
    return true;
}

// Comment

static inline bool scan_comment(TSLexer *lexer, bool start_of_line) {
    while (lexer->lookahead == ' ')
        skip(lexer);

    if (lexer->lookahead != '#')
        return false;
    advance(lexer);

    // This is a shebang
    if (start_of_line && lexer->lookahead == '!')
        return false;

    while (lexer->lookahead != '\n' && !lexer->eof(lexer)) {
        advance(lexer);
    }
    lexer->result_symbol = COMMENT;
    return true;
}

// Infinities and Nan

static inline bool scan_inf(TSLexer *lexer) {
    if (test_char_range(lexer->lookahead, "+-"))
        advance(lexer);

    if (!test_word(lexer, "inf", false))
        return false;

    if (test_char_range(lexer->lookahead, "iI")) {
        advance(lexer);
        if (!test_word(lexer, "nity", false))
            return false;
    };

    if (!common_terminator(lexer))
        return false;

    lexer->result_symbol = MATH_KEYWORD;
    return true;
}

static inline bool scan_nan(TSLexer *lexer) {
    if (!test_word(lexer, "nan", false))
        return false;

    if (!common_terminator(lexer))
        return false;

    lexer->result_symbol = MATH_KEYWORD;
    return true;
}

// Tree-sitter scanner object

bool tree_sitter_nu_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
    // The documentation states that if the lexical analysis fails for some
    // reason they will mark every state as valid and pass it to the external
    // scanner However, we can't do anything to help them recover in that case
    // so we should just fail.
    /*
      link:
      https://tree-sitter.github.io/tree-sitter/creating-parsers#external-scanners
      If a syntax error is encountered during regular parsing, Tree-sitter’s
      first action during error recovery will be to call the external scanner’s
      scan function with all tokens marked valid. The scanner should detect this
      case and handle it appropriately. One simple method of detection is to add
      an unused token to the end of the externals array, for example

      externals: $ => [$.token1, $.token2, $.error_sentinel],

      then check whether that token is marked valid to determine whether
      Tree-sitter is in error correction mode.
    */
    if (valid_symbols[ERROR_SENTINEL]) {
        return false;
    }

    Scanner *scanner = (Scanner *)payload;

    if (valid_symbols[RAW_STRING_LITERAL_START] && (lexer->lookahead == 'r')) {
        return scan_raw_string_start(scanner, lexer);
    }

    if (valid_symbols[RAW_STRING_LITERAL_CONTENT]) {
        return scan_raw_string_content(scanner, lexer);
    }

    if (valid_symbols[RAW_STRING_LITERAL_END] &&
        lexer->lookahead == RAW_STRING_QUOTE_CHAR) {
        return scan_raw_string_end(scanner, lexer);
    }

    if (valid_symbols[MATH_KEYWORD] &&
        test_char_range(lexer->lookahead, "+-iI")) {
        return scan_inf(lexer);
    }
    if (valid_symbols[MATH_KEYWORD] &&
        test_char_range(lexer->lookahead, "nN")) {
        return scan_nan(lexer);
    }

    bool start_of_line = (lexer->get_column(lexer) == 0);

    if (valid_symbols[COMMENT] && (lexer->lookahead == ' ' || start_of_line)) {
        return scan_comment(lexer, start_of_line);
    }

    return false;
}
