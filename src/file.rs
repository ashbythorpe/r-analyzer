use log::info;
use lsp_types::TextDocumentContentChangeEvent;
use ropey::{Rope, RopeSlice};

use crate::{
    grammar::{FilePosition, FileSpan, Token},
    lexer::{lex, LexerError},
    nodes::Node,
    parser::{parse, ParseError},
};

pub struct SourceFile {
    content: Rope,
    tokens: Vec<Token>,
    parse_tree: Node,
    lexer_errors: Vec<LexerError>,
    parse_errors: Vec<ParseError>,
}

impl SourceFile {
    fn new(
        content: Rope,
        tokens: Vec<Token>,
        parse_tree: Node,
        lexer_errors: Vec<LexerError>,
        parse_errors: Vec<ParseError>,
    ) -> Self {
        Self {
            content,
            tokens,
            parse_tree,
            lexer_errors,
            parse_errors,
        }
    }

    pub fn parse(content: Rope) -> Self {
        info!("Lexing file");
        let (tokens, lexer_errors) = lex(&content);

        info!("Lexing result: {:#?}", tokens);

        info!("Parsing tokens");
        let (parse_tree, parse_errors) = parse(&tokens);

        info!("Parse result: {:?}", parse_tree);
        Self::new(content, tokens, parse_tree, lexer_errors, parse_errors)
    }

    pub fn get_content(&self) -> &Rope {
        &self.content
    }

    pub fn get_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn get_parse_tree(&self) -> &Node {
        &self.parse_tree
    }

    pub fn lexer_errors(&self) -> &[LexerError] {
        &self.lexer_errors
    }

    pub fn parse_errors(&self) -> &[ParseError] {
        &self.parse_errors
    }

    pub fn update(&mut self, changes: Vec<TextDocumentContentChangeEvent>) {
        for change in changes {
            if let Some(range) = change.range {
                let span: FileSpan = range.into();
                let (start, end) = span.get_char_span(&self.content);
                self.content.remove(start..end);
                self.content.insert(start, &change.text);
            } else {
                self.content = Rope::from_str(&change.text);
            }
        }

        (self.tokens, self.lexer_errors) = lex(&self.content);

        (self.parse_tree, self.parse_errors) = parse(&self.tokens);
    }

    pub fn token_at(&self, position: FilePosition) -> &Token {
        self.tokens
            .iter()
            .find(|x| x.span().contains(position))
            .unwrap()
    }

    pub fn token_index_at(&self, position: FilePosition) -> usize {
        self.tokens
            .iter()
            .position(|x| x.span().contains(position))
            .unwrap()
    }

    pub fn whole_document(&self) -> FileSpan {
        let lines = self.content.len_lines();
        let line_pos = self.content.line_to_char(lines - 1);
        FileSpan::new(
            FilePosition::new(0, 0),
            FilePosition::new(lines, self.content.len_chars() - line_pos),
        )
    }

    pub fn get_node_text(&self, node: &Node) -> Option<RopeSlice> {
        let (start, end) = node.text_span(&self.tokens)?.get_char_span(&self.content);

        Some(self.content.slice(start..end))
    }

    pub fn get_file_span(&self, span: &FileSpan) -> RopeSlice {
        let (start, end) = span.get_char_span(&self.content);
        self.content.slice(start..end)
    }
}
