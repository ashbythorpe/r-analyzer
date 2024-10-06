use anyhow::{anyhow, Result};

use crate::{
    file::SourceFile,
    grammar::{FilePosition, FileSpan, Span, Token},
    nodes::Node,
};

pub struct Cursor<'a> {
    current: &'a Node,
    parents: Vec<&'a Node>,
}

pub struct NoParentError;

pub enum SiblingError {
    NoMoreSiblings,
    NoParent(NoParentError),
}

impl<'a> Cursor<'a> {
    pub fn new(root: &'a Node) -> Self {
        Self {
            current: root,
            parents: Vec::new(),
        }
    }

    pub fn current(&self) -> &'a Node {
        self.current
    }

    pub fn children(&self) -> &'a Vec<Node> {
        self.current.children()
    }

    pub fn parent(&self) -> Option<&'a Node> {
        self.parents.last().copied()
    }

    pub fn go_to_child(&mut self, child: &'a Node) {
        self.parents.push(child);
        self.current = child;
    }

    pub fn go_to_parent(&mut self) -> Result<(), NoParentError> {
        self.current = self.parents.pop().ok_or(NoParentError)?;

        Ok(())
    }

    pub fn go_to_next_sibling(&mut self) -> Result<(), SiblingError> {
        let parent = self
            .parents
            .last()
            .ok_or(SiblingError::NoParent(NoParentError))?;

        let index = parent
            .children()
            .iter()
            .position(|x| std::ptr::eq(x, self.current))
            .expect("Parent does not contain child");

        if index == parent.children().len() - 1 {
            return Err(SiblingError::NoMoreSiblings);
        }

        self.go_to_child(&parent.children()[index + 1]);
        Ok(())
    }

    pub fn go_to_previous_sibling(&mut self) -> Result<(), SiblingError> {
        let parent = self
            .parents
            .last()
            .ok_or(SiblingError::NoParent(NoParentError))?;

        let index = parent
            .children()
            .iter()
            .position(|x| std::ptr::eq(x, self.current))
            .expect("Parent does not contain child");

        if index == 0 {
            return Err(SiblingError::NoMoreSiblings);
        }

        self.go_to_child(&parent.children()[index - 1]);
        Ok(())
    }

    pub fn reset(&mut self) {
        if let Some(&root) = self.parents.first() {
            self.current = root;
            self.parents.clear();
        }
    }

    pub fn at_leaf(&self) -> bool {
        self.current.is_leaf()
    }

    pub fn span(&self) -> &Span {
        self.current.span()
    }

    pub fn text_span(&self, tokens: &Vec<Token>) -> Option<FileSpan> {
        self.current.text_span(tokens)
    }
}

pub fn node_at_position(file: &SourceFile, position: FilePosition) -> Result<Cursor> {
    let parse_tree = file.get_parse_tree();
    let tokens = file.get_tokens();

    let mut cursor = Cursor::new(parse_tree);

    while !cursor.at_leaf() {
        let child = cursor
            .children()
            .iter()
            .find(|x| x.contains(position, tokens))
            .unwrap();

        cursor.go_to_child(child);
    }

    Ok(cursor)
}

pub fn node_covering(file: &SourceFile, span: FileSpan) -> Result<Cursor> {
    let parse_tree = file.get_parse_tree();
    let tokens = file.get_tokens();

    let mut cursor = Cursor::new(parse_tree);

    while let Some(x) = cursor.children().iter().find(|x| x.covers(span, tokens)) {
        cursor.go_to_child(x)
    }

    Ok(cursor)
}
