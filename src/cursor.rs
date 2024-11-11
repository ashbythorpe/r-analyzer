use anyhow::Result;
use log::info;

use crate::{
    file::SourceFile,
    grammar::{FilePosition, FileSpan, Span, Token},
    nodes::Node,
};

#[derive(Clone, Debug)]
pub struct Cursor<'a> {
    current: &'a Node,
    parents: Vec<(&'a Node, usize)>,
}

#[derive(Debug)]
pub struct NoParentError;

#[derive(Debug)]
pub enum SiblingError {
    NoMoreSiblings,
    NoParent(NoParentError),
}

pub struct NotEnoughChildrenError;

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

    pub fn children(&self) -> Vec<&'a Node> {
        self.current.children()
    }

    pub fn parent(&self) -> Option<&'a Node> {
        self.parents.last().map(|x| x.0)
    }

    pub fn go_to_child(&mut self, child: &'a Node) {
        let index = self
            .current
            .children()
            .iter()
            .position(|&x| x == child)
            .expect("Could not find child");

        self.parents.push((self.current, index));
        self.current = child;
    }

    pub fn to_child(&self, child: &'a Node) -> Self {
        let mut cursor = self.clone();
        cursor.go_to_child(child);
        cursor
    }

    pub fn go_to_child_index(&mut self, index: usize) -> Result<(), NotEnoughChildrenError> {
        let children = self.current.children();

        let child = match children.get(index) {
            Some(x) => x,
            None => return Err(NotEnoughChildrenError),
        };

        self.go_to_child(child);
        Ok(())
    }

    pub fn go_to_parent(&mut self) -> Result<(), NoParentError> {
        (self.current, _) = self.parents.pop().ok_or(NoParentError)?;

        Ok(())
    }

    pub fn go_to_next_sibling(&mut self) -> Result<(), SiblingError> {
        let (parent, index) = self
            .parents
            .last()
            .ok_or(SiblingError::NoParent(NoParentError))?;

        if *index == parent.children().len() - 1 {
            return Err(SiblingError::NoMoreSiblings);
        }

        self.current = parent.children()[index + 1];
        self.parents.last_mut().unwrap().1 += 1;
        Ok(())
    }

    pub fn go_to_previous_sibling(&mut self) -> Result<(), SiblingError> {
        let (parent, index) = self
            .parents
            .last()
            .ok_or(SiblingError::NoParent(NoParentError))?;

        if *index == 0 {
            return Err(SiblingError::NoMoreSiblings);
        }

        self.current = parent.children()[index - 1];
        self.parents.last_mut().unwrap().1 -= 1;
        Ok(())
    }

    pub fn reset(&mut self) {
        if let Some((root, _)) = self.parents.first() {
            self.current = root;
            self.parents.clear();
        }
    }

    pub fn at_leaf(&self) -> bool {
        self.current.is_leaf()
    }

    pub fn span(&self) -> Option<&Span> {
        self.current.span()
    }

    pub fn text_span(&self, tokens: &[Token]) -> Option<FileSpan> {
        self.current.text_span(tokens)
    }

    pub fn siblings(&self) -> Vec<&'a Node> {
        if let Some(parent) = self.parent() {
            parent.children()
        } else {
            vec![self.current]
        }
    }

    pub fn is_top_level(&self) -> bool {
        self.parents.len() <= 1
    }
}

pub fn go_to_node<'a>(file: &'a SourceFile, node: &'a Node) -> Cursor<'a> {
    let mut cursor = Cursor::new(file.get_parse_tree());
    let span = node.span().expect("Node must have a span");

    while cursor.current() != node {
        let children = cursor.children();

        let child = children
            .iter()
            .find(|x| x.span().is_some_and(|x| x.includes(span)))
            .unwrap();

        cursor.go_to_child(child);
    }

    cursor
}

pub fn node_at_position(file: &SourceFile, position: FilePosition) -> Cursor {
    let parse_tree = file.get_parse_tree();
    let tokens = file.get_tokens();

    info!("Position: {:?}", position);

    let mut cursor = Cursor::new(parse_tree);

    loop {
        let children = cursor.children();
        let child = match children.iter().find(|x| x.contains(position, tokens)) {
            Some(x) => x,
            None => return cursor,
        };

        cursor.go_to_child(child);
    }
}

pub fn node_covering(file: &SourceFile, span: FileSpan) -> Cursor {
    let parse_tree = file.get_parse_tree();
    let tokens = file.get_tokens();

    let mut cursor = Cursor::new(parse_tree);

    while let Some(x) = cursor.children().iter().find(|x| x.covers(span, tokens)) {
        cursor.go_to_child(x)
    }

    cursor
}
