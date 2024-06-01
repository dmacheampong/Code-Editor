/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/

#define TXT_EDIT_VERSION "0.0.1"
#define TXT_EDIT_TAB_STOP 4
#define TXT_EDIT_QUIT_TIMES 3
#define CURSOR_X_OFFSET 4

#define CTRL_KEY(k) ((k) & 0x1f) // Defines CTRL key combinations

enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,    
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHTLIGHT_STRINGS (1<<1)

/*** data ***/

struct FileSyntax {
    char *file_type;
    char **filematch;
    char **keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;
};

typedef struct Row {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *rendered_chars;
    unsigned char *hl;
    int hl_open_comment;
} Row;

struct Editor {
    int cursor_x, cursor_y; // Cursor positions
    int rendered_x; // Render x position for tabs
    int row_offset;
    int col_offset;
    int screen_rows;
    int screen_cols;
    int num_rows;
    Row *rows;
    int changes; 
    char *filename;
    char status_msg[80];
    time_t status_msg_time;
    struct FileSyntax *syntax;
    struct termios orig_termios;
};

struct Editor editor;

/*** file types ***/

char *c_extensions[] = { ".c", ".h", ".cpp", NULL };
char *java_extensions[] = {".java", NULL};
char *js_extensions[] = { ".js", ".ts", ".jsx", "tsx", NULL };
char *python_extensions[] = { ".py", NULL };

char *c_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case", "#include", "#define"
    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

char *java_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case", "import"
    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

char *js_keywords[] = {
    "async", "await", "const|", "default", "else", "export", "function", "if", "import",
    "let|", "return", "try", "var|", NULL
};

char *python_keywords[] = {
	"def", "elif", "if", "while", "for", NULL
};

struct FileSyntax hightlightDatabase[] = { // Highlight database
    {
        "c",
        c_extensions,
        c_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHTLIGHT_STRINGS
    },
    {
        "java",
        java_extensions,
        java_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHTLIGHT_STRINGS
    },
    {
        "js",
        js_extensions,
        js_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHTLIGHT_STRINGS
    },

    {
        "py",
        python_extensions,
        python_keywords,
        "#", "'''", "'''",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHTLIGHT_STRINGS
    }
};

#define HLDB_ENTRIES (sizeof(hightlightDatabase) / sizeof(hightlightDatabase[0]))

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen(void);
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** terminal ***/

void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4); // Clears screen
    write(STDOUT_FILENO, "\x1b[H", 3); // Postions cursor at first row and column
    
    perror(s);
    exit(1);
}

void disableRawMode(void) {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &editor.orig_termios) == -1) {
        die("tcsetattr");
    }
}

void enableRawMode(void) {
    if (tcgetattr(STDIN_FILENO, &editor.orig_termios) == -1) {
        die("tcgetattr");
    }
    atexit(disableRawMode);

    struct termios raw = editor.orig_termios; //Set struct to modify
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST); //Turn off all output processing
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG); //Forces fourth bit in local flag to be 0
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) { //Passes back modified struct
        die("tcsetattr");
    } 
}

// Waits for one keypress and returns it
int editorReadKey(void) {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }

    if (c == '\x1b') { // Checks if it's an escape code.
        char seq[3]; //Stores 3 bytes

        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if (seq[0] == '[') { // Checks if Control Squence Introducer
            if (seq[1] >= '0' && seq[1] <= '9') { // Checks CSI arguments
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            } else {
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }
        return '\x1b';
    } else {
        return c;
    }
}

/**
 * @brief Get the cursor position
 * 
 * @param int rows 
 * @param int cols 
 * @return int 
 */
int getCursorPosition(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;
    
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    if (buf[0] != '\x1b' || buf[1] != '[') return -1; //Makes sures it's an esc sequence
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
    return -1;
}

/**
 * @brief Get the window size
 * 
 * @param rows 
 * @param cols 
 * @return int 
 */
int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*** syntax highlighting ***/

int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(Row *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (editor.syntax == NULL) return;

    char **keywords = editor.syntax->keywords;

    char *scs = editor.syntax->singleline_comment_start;
    char *mcs = editor.syntax->multiline_comment_start;
    char *mce = editor.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs): 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && editor.rows[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->rsize) {
        char c = row->rendered_chars[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->rendered_chars[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->rendered_chars[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->rendered_chars[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (editor.syntax->flags & HL_HIGHTLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) in_string = 0;
                i++;
                prev_sep= 1;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (editor.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int is_kw2 = keywords[j][klen - 1] == '|';
                if (is_kw2) klen--;

                if (!strncmp(&row->rendered_chars[i], keywords[j], klen) &&
                    is_separator(row->rendered_chars[i + klen])) {
                    memset(&row->hl[i], is_kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    int is_changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (is_changed && row->idx + 1 < editor.num_rows)
        editorUpdateSyntax(&editor.rows[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
    switch (hl) {
        case HL_COMMENT:
        case HL_MLCOMMENT: return 32; // Green
        case HL_KEYWORD1: return 35; // Magenta
        case HL_KEYWORD2: return 36; // Cyan
        case HL_STRING: return 33; // Yellow
        case HL_NUMBER: return 31; // Red
        case HL_MATCH: return 34;
        default: return 37; // White
    }
}

void editorSelectSyntaxHighlight(void) {
    editor.syntax = NULL;
    if (editor.filename == NULL) return;

    char *ext = strrchr(editor.filename, '.');
    
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct FileSyntax *s = &hightlightDatabase[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                (!is_ext && strstr(editor.filename, s->filematch[i]))) {
                editor.syntax = s;

                int file_row;
                for (file_row = 0; file_row < editor.num_rows; file_row++) {
                    editorUpdateSyntax(&editor.rows[file_row]);
                }
                return;
            }
            i++;
        }
    }
}

/*** row operations ***/

int editorRowCxToRx(Row *row, int cursor_x) {
    int rendered_x = 0;
    int i;
    for (i = 0; i < cursor_x; i++) {
        if (row->chars[i] == '\t')
            rendered_x += (TXT_EDIT_TAB_STOP - 1) - (rendered_x % TXT_EDIT_TAB_STOP);
        rendered_x++;
    }
    return rendered_x;
}

int editorRowRxToCx(Row *row, int rx) {
    int curr_rendered_x = 0;
    int cursor_x;
    for (cursor_x = 0; cursor_x < row->size; cursor_x++) {
        if (row->chars[cursor_x] == '\t')
            curr_rendered_x += (TXT_EDIT_TAB_STOP - 1) - (curr_rendered_x % TXT_EDIT_TAB_STOP);
        curr_rendered_x++;

        if (curr_rendered_x > rx) return cursor_x;
    }
    return cursor_x;
}

void editorUpdateRow(Row *row) {
    int tabs = 0;
    int j;
    for (j = 0; j < row->size; j++)
        if (row->chars[j] == '\t') tabs++; // Gets number of tab chars

    free(row->rendered_chars);
    row->rendered_chars = malloc(row->size + (tabs * (TXT_EDIT_TAB_STOP - 1)) + 1);

    int i = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->rendered_chars[i++] = ' ';
            while (i % TXT_EDIT_TAB_STOP != 0) row->rendered_chars[i++] = ' ';
        } else {
            row->rendered_chars[i++] = row->chars[j];
        }
    }
    row->rendered_chars[i] = '\0';
    row->rsize = i;

    editorUpdateSyntax(row);
}

/*
    Inserts new row in editor.
*/
void editorInsertRow(int i, char *s, size_t len) {
    if (i < 0 || i > editor.num_rows) return;

    editor.rows = realloc(editor.rows, sizeof(Row) * (editor.num_rows + 1));
    memmove(&editor.rows[i + 1], &editor.rows[i], sizeof(Row) * (editor.num_rows - i));
    for (int j = i + 1; j <= editor.num_rows; j++) editor.rows[j].idx++;

    editor.rows[i].idx = i;

    editor.rows[i].size = len;
    editor.rows[i].chars = malloc(len + 1);
    memcpy(editor.rows[i].chars, s, len);
    editor.rows[i].chars[len] = '\0';

    editor.rows[i].rsize = 0;
    editor.rows[i].rendered_chars = NULL;
    editor.rows[i].hl = NULL;
    editor.rows[i].hl_open_comment = 0;
    editorUpdateRow(&editor.rows[i]);

    editor.num_rows++;
    editor.changes++;
}

/*
    Frees memory space from row in editor.
*/
void editorFreeRow(Row *row) {
    free(row->rendered_chars);
    free(row->chars);
    free(row->hl);
}

/**
 * @brief 
 * Deletes row in editor.
 * @param int i 
 */
void editorDelRow(int i) {
    if (i < 0 || i >= editor.num_rows) return;
    editorFreeRow(&editor.rows[i]);
    memmove(&editor.rows[i], &editor.rows[i + 1], sizeof(Row) * (editor.num_rows - i - 1));
    for (int j = i; j < editor.num_rows - 1; j++) editor.rows[j].idx--;
    editor.num_rows--;
    editor.changes++;
}

void editorRowInsertChar(Row *row, int i, int c) {
    if (i < 0 || i > row->size) i = row->size;
    row->chars = realloc(row->chars, row->size + 2); // Add one byte for the char and one for the null byte
    memmove(&row->chars[i + 1], &row->chars[i], row->size - i + 1);
    row->size++;
    row->chars[i] = c;
    editorUpdateRow(row);
    editor.changes++;
}

void editorRowDelChar(Row *row, int i) {
    if (i < 0 || i >= row->size) return;
    memmove(&row->chars[i], &row->chars[i + 1], row->size - i);
    row->size--;
    editorUpdateRow(row);
    editor.changes++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
    if (editor.cursor_y == editor.num_rows) {
        editorInsertRow(editor.num_rows, "", 0);
    }
    editorRowInsertChar(&editor.rows[editor.cursor_y], editor.cursor_x, c);
    editor.cursor_x++;
}

void editorInsertNewline(void) {
    if (editor.cursor_x == 0) {
        editorInsertRow(editor.cursor_y, "", 0);
    } else { // If in the middle of a line.
        Row *row = &editor.rows[editor.cursor_y];
        editorInsertRow(editor.cursor_y + 1, &row->chars[editor.cursor_x], row->size - editor.cursor_x);
        row = &editor.rows[editor.cursor_y];
        row->size = editor.cursor_x;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }
    editor.cursor_y++;
    editor.cursor_x = 0;
}

void editorRowAppendString(Row *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    editor.changes++;
}

void editorDelChar(void) {
    if (editor.cursor_y == editor.num_rows) return;
    if (editor.cursor_x == 0 && editor.cursor_y == 0) return;

    Row *row = &editor.rows[editor.cursor_y];
    if (editor.cursor_x > 0) {
        editorRowDelChar(row, editor.cursor_x - 1);
        editor.cursor_x--;
    } else {
        editor.cursor_x = editor.rows[editor.cursor_y - 1].size;
        editorRowAppendString(&editor.rows[editor.cursor_y - 1], row->chars, row->size);
        editorDelRow(editor.cursor_y);
        editor.cursor_y--;
    }
}

/*** file i/o ***/

char *editorRowsToString(int *buf_len) {
    int total_len = 0;
    int i;
    for (i = 0; i < editor.num_rows; i++)
        total_len += editor.rows[i].size + 1; //Add up length of each row + 1 for '\n'
    *buf_len = total_len;

    char *buf = malloc(total_len);
    char *buf_ptr = buf;
    for (i = 0; i < editor.num_rows; i++) {
        memcpy(buf_ptr, editor.rows[i].chars, editor.rows[i].size);
        buf_ptr += editor.rows[i].size;
        *buf_ptr = '\n';
        buf_ptr++;
    }

    return buf;
}

void editorOpen(char *filename) {
    free(editor.filename);
    editor.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen-1] == '\r'))
            linelen--;
        editorInsertRow(editor.num_rows, line, linelen);
    }
    free(line);
    fclose(fp);
    editor.changes = 0;
}

void editorSave(void) {
    if (editor.filename == NULL) {
        editor.filename = editorPrompt("Save as: %s (ESC to cancel).", NULL);
        if (editor.filename == NULL) {
            editorSetStatusMessage("Save aborted.");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buffer = editorRowsToString(&len);

    int fd = open(editor.filename, O_RDWR | O_CREAT, 0644); //0644 standaard permissions
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buffer, len) == len) {
                close(fd);
                free(buffer);
                editor.changes = 0;
                editorSetStatusMessage("%d bytes written to disk.", len);
                return;
            }
        }
        close(fd);
    }
    free(buffer);
    editorSetStatusMessage("Save failed! I/O error: %s.", strerror(errno));
}

/*** find ***/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(editor.rows[saved_hl_line].hl, saved_hl, editor.rows[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;
    int i;
    for (i = 0; i < editor.num_rows; i++) {
        current += direction;
        if (current == -1) current = editor.num_rows - 1;
        else if (current == editor.num_rows) current = 0;

        Row *row = &editor.rows[current];
        char *match = strstr(row->rendered_chars, query);
        if (match) {
            last_match = current;
            editor.cursor_y = current;
            editor.cursor_x = editorRowRxToCx(row, match - row->rendered_chars);
            editor.row_offset = editor.num_rows;

            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->rendered_chars], HL_MATCH, strlen(query));
            break;
        }
    } 
}

void editorFind(void) {
    int saved_cx = editor.cursor_x;
    int saved_cy = editor.cursor_y;
    int saved_col_offset = editor.col_offset;
    int saved_row_offset = editor.row_offset;

    char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
                                editorFindCallback);
    
    if (query) {
        free(query);
    } else {
        editor.cursor_x = saved_cx;
        editor.cursor_y = saved_cy;
        editor.col_offset = saved_col_offset;
        editor.row_offset = saved_row_offset;
    }
}

/*** append buffer ***/

struct AppendBuffer {
    char *buf; // Char buffer
    int len; // Length of buffer
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct AppendBuffer *ab, const char *s, int len) {
    char *new_buf = realloc(ab->buf, ab->len + len);

    if (new_buf == NULL) return;
    memcpy(&new_buf[ab->len], s, len);
    ab->buf = new_buf;
    ab->len += len;
}

void abFree(struct AppendBuffer *ab) {
    free(ab->buf);
}

/*** output ***/

void editorScroll(void) {
    editor.rendered_x = 0;

    if (editor.cursor_y < editor.num_rows) {
        editor.rendered_x = editorRowCxToRx(&editor.rows[editor.cursor_y], editor.cursor_x);
    }
    if (editor.cursor_y < editor.row_offset) {
        editor.row_offset = editor.cursor_y;
    }
    if (editor.cursor_y >= editor.row_offset + editor.screen_rows) {
        editor.row_offset = editor.cursor_y - editor.screen_rows + 1;
    }
    if (editor.rendered_x < editor.col_offset) {
        editor.col_offset = editor.rendered_x;
    }
    if (editor.rendered_x >= editor.col_offset + editor.screen_cols) {
        editor.col_offset = editor.rendered_x - editor.screen_cols + 1;
    }
}

void editorDrawRows(struct AppendBuffer *ab) {
    int y;
    for (y = 0; y < editor.screen_rows; y++) {
        int file_row = y + editor.row_offset;
        if (file_row >= editor.num_rows) {
            if (editor.num_rows == 0 && y == editor.screen_rows / 3) { // If file is blank and has no rows
                char welcome_msg[80];
                int msg_len = snprintf(welcome_msg, sizeof(welcome_msg),
                    "TXT Editor -- version %s", TXT_EDIT_VERSION);
                if (msg_len > editor.screen_cols) msg_len = editor.screen_cols;
                int padding = (editor.screen_cols - msg_len) / 2;
                if (padding) {
                    abAppend(ab, "~", 1);
                    padding--;
                }
                while (padding--) abAppend(ab, " ", 1);
                abAppend(ab, welcome_msg, msg_len);
            } else {
                abAppend(ab, "~", 1);
            }
        } else {         
            //char line_num[10];
            //int digit_cnt = snprintf(line_num, sizeof(line_num), "%-4d", editor.rows[file_row].idx + 1);
            //abAppend(ab, line_num, digit_cnt);
            int len = editor.rows[file_row].rsize - editor.col_offset;
            if (len < 0) len = 0;
            if (len > editor.screen_cols) len = editor.screen_cols;
            char *c = &editor.rows[file_row].rendered_chars[editor.col_offset];
            unsigned char *hl = &editor.rows[file_row].hl[editor.col_offset];
            int current_color = -1;
            int j;
            for (j = 0; j < len; j++) {
                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j]: '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (current_color != -1) {
                        char buf[16];
                        int clen 
                        = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != - 1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }
        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct AppendBuffer *ab) {
    abAppend(ab, "\x1b[7m", 4);
    char status_left[80], status_right[80];
    int status_left_len = snprintf(status_left, sizeof(status_left), "%.20s - %d lines %s",
        editor.filename ? editor.filename : "[No Name]", editor.num_rows,
        editor.changes ? "(modified)" : "");
    int status_right_len = snprintf(status_right, sizeof(status_right), "%s | %d/%d", 
        editor.syntax ? editor.syntax->file_type : "no ft", editor.cursor_y + 1, editor.num_rows);
    if (status_left_len > editor.screen_cols) status_left_len = editor.screen_cols;
    abAppend(ab, status_left, status_left_len);
    while (status_left_len < editor.screen_cols) {
        if (editor.screen_cols - status_left_len == status_right_len) {
            abAppend(ab, status_right, status_right_len);
            break;
        } else {
            abAppend(ab, " ", 1);
            status_left_len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct AppendBuffer *ab) {
    abAppend(ab, "\x1b[K", 3); // <esc>[K Clears message bar
    int msg_len = strlen(editor.status_msg);
    if (msg_len > editor.screen_cols) msg_len = editor.screen_cols;
    if (msg_len && time(NULL) - editor.status_msg_time < 5)
        abAppend(ab, editor.status_msg, msg_len);
}

void editorRefreshScreen(void) {
    editorScroll();
    struct AppendBuffer ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);
    abAppend(&ab, "\x1b[H", 3); //Postions cursor at first row and column

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);
    
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", 
    (editor.cursor_y - editor.row_offset) + 1,
    (editor.rendered_x - editor.col_offset) + 1);

    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6);

    write(STDOUT_FILENO, ab.buf, ab.len);
    abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(editor.status_msg, sizeof(editor.status_msg), fmt, ap);
    va_end(ap);
    editor.status_msg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0 ) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) callback(buf, c);
    }
}

void editorMoveCursor(int key) {
    Row *row = (editor.cursor_y >= editor.num_rows) ? NULL : &editor.rows[editor.cursor_y];

    switch (key) {
        case ARROW_LEFT:
            if (editor.cursor_x != 0) {
                editor.cursor_x--;
            } else if (editor.cursor_y > 0) {
                editor.cursor_y--;
                editor.cursor_x = editor.rows[editor.cursor_y].size;
            }
            break;
        case ARROW_RIGHT:
            if (row && editor.cursor_x < row->size) {
                editor.cursor_x++;
            } else if (row && editor.cursor_x == row->size) {
                editor.cursor_y++;
                editor.cursor_x = 0;
            }
            break;
        case ARROW_UP:
            if (editor.cursor_y != 0) {
                editor.cursor_y--;
            }
            break;
        case ARROW_DOWN:
            if (editor.cursor_y < editor.num_rows) {
                editor.cursor_y++;
            }
            break;
    }

    row = (editor.cursor_y >= editor.num_rows) ? NULL : &editor.rows[editor.cursor_y];
    int row_len = row ? row->size : 0;
    if (editor.cursor_x > row_len) {
        editor.cursor_x = 0;
    }
}

//Waits for keypress and handles it
void editorProcessKeypress(void) {
    static int quit_times = TXT_EDIT_QUIT_TIMES;

    int c = editorReadKey();

    switch (c) {
        case '\r':
            editorInsertNewline();
            break;

        case CTRL_KEY('q'):
            if (editor.changes && quit_times > 0) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes! "
                "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4); //Clears screen
            write(STDOUT_FILENO, "\x1b[H", 3); //Postions cursor at first row and column
            exit(0);
            break;

        case CTRL_KEY('s'):
            editorSave();
            break;
    
        case HOME_KEY:
            editor.cursor_x = 0;
            break;

        case END_KEY:
            if (editor.cursor_y < editor.num_rows)
                editor.cursor_x = editor.rows[editor.cursor_y].size;
            break;
    
        case CTRL_KEY('f'):
            editorFind();
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
            editorDelChar();
            break;
        
        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (c == PAGE_UP) {
                    editor.cursor_y = editor.row_offset;
                } else if (c == PAGE_DOWN) {
                    editor.cursor_y = editor.row_offset + editor.screen_rows - 1;
                    if (editor.cursor_y > editor.num_rows) editor.cursor_y = editor.num_rows;
                }
                int times = editor.screen_rows;
                while (times--)
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);   
            }
            break;
        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            break;
        
        default:
            editorInsertChar(c);
            break;
    }

    quit_times = TXT_EDIT_QUIT_TIMES;
}

/*** init ***/

void initEditor(void) {
    editor.cursor_x = 0; // Initial x position for cursor offset for line numbers.
    editor.cursor_y = 0; // Initial y position for cursor.
    editor.rendered_x = 0;
    editor.row_offset = 0;
    editor.col_offset = 0;
    editor.num_rows = 0;
    editor.rows = NULL;
    editor.changes = 0;
    editor.filename = NULL;
    editor.status_msg[0] = '\0';
    editor.status_msg_time = 0;
    editor.syntax = NULL;

    if (getWindowSize(&editor.screen_rows, &editor.screen_cols) == -1) die("getWindowSize");
    editor.screen_rows -= 2;
}

int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage("HELP: Ctrl-S = Save | Ctrl-Q = Quit | Ctrl-F = Find");

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}
