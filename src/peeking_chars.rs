use std::str::Chars;


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

    pub fn next_if_matches(&mut self, accept: &str) -> Option<char> {
        self.next_if(|x| accept.contains(x))
    }

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

    pub fn stored_string(&self) -> &str {
        &self.stored_string
    }

    pub fn take_stored_string(&mut self) -> String {
        self.start_line = self.line;
        self.start_column = self.column;
        self.stored_string.drain(..).collect()
    }
}
