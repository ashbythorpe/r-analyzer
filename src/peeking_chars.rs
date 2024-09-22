use std::str::Chars;

use crate::grammar::FileSpan;

/// A wrapper around [Chars] that allows for a few operations that are useful during lexing.
pub struct CharTraverser<'a> {
    chars: Chars<'a>,
    stored_string: String,
    next: String,
    start_line: usize,
    start_column: usize,
    line: usize,
    column: usize,
}

impl<'a> CharTraverser<'a> {
    pub fn new(input: &'a str) -> Self {
        CharTraverser {
            chars: input.chars(),
            stored_string: String::new(),
            next: String::new(),
            start_line: 0,
            start_column: 0,
            line: 0,
            column: 0,
        }
    }

    /// Get the next character
    pub fn next(&mut self) -> Option<char> {
        let next = match self.next.pop() {
            Some(x) => x,
            None => self.chars.next()?,
        };

        if next == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        self.stored_string.push(next);

        Some(next)
    }

    /// Get the next character if it matches a condition
    pub fn next_if(&mut self, mut accept: impl FnMut(char) -> bool) -> Option<char> {
        let next = match self.next.pop() {
            Some(x) => x,
            None => self.chars.next()?,
        };

        if accept(next) {
            if next == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }

            self.stored_string.push(next);
            Some(next)
        } else {
            self.next.push(next);
            None
        }
    }

    /// Get the next character if it matches any of the characters in `accept`
    pub fn next_if_matches(&mut self, accept: &str) -> Option<char> {
        self.next_if(|x| accept.contains(x))
    }

    /// Get the next character while it matches a condition
    pub fn next_while(&mut self, mut accept: impl FnMut(char) -> bool) {
        let mut next = match self.next.pop() {
            Some(x) => x,
            None => match self.chars.next() {
                Some(x) => x,
                None => return,
            },
        };

        while accept(next) {
            if next == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }

            self.stored_string.push(next);

            next = match self.next.pop() {
                Some(x) => x,
                None => match self.chars.next() {
                    Some(x) => x,
                    None => return,
                },
            };
        }

        self.next.push(next);
    }

    /// Get the next character while it matches any of the characters in `accept`,
    /// and return the number of characters that were consumed.
    pub fn count_next(&mut self, accept: impl Fn(char) -> bool) -> usize {
        let mut count = 0;

        let mut next = match self.next.pop() {
            Some(x) => x,
            None => match self.chars.next() {
                Some(x) => x,
                None => return count,
            },
        };

        while accept(next) {
            count += 1;

            if next == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }

            self.stored_string.push(next);

            next = match self.next.pop() {
                Some(x) => x,
                None => match self.chars.next() {
                    Some(x) => x,
                    None => return count,
                },
            };
        }

        self.next.push(next);

        count
    }

    /// Get the string of all the characters that have been read so far
    pub fn stored_string(&self) -> &str {
        &self.stored_string
    }

    /// Take the string of all the characters that have been read so far, along with
    /// the span of characters that it represents
    ///
    /// Resets the stored string to an empty string
    pub fn take_stored_string(&mut self) -> (String, FileSpan) {
        let span = FileSpan::new(self.start_line, self.start_column, self.line, self.column);
        self.start_line = self.line;
        self.start_column = self.column;
        (self.stored_string.drain(..).collect(), span)
    }
}
