pub struct SourceFile {
    content: String,
    newlines: Vec<usize>,
}

impl SourceFile {
    fn new(content: String) -> SourceFile {
        let newlines = newlines_cumsum(&content);
        SourceFile { content, newlines }
    }

    pub fn get_content(&self) -> &str {
        &self.content
    }
}

fn newlines_cumsum(content: &str) -> Vec<usize> {
    content
        .split_inclusive('\n')
        .map(|x| x.len())
        .scan(0, |sum, x| {
            let prev_sum = *sum;
            *sum += x;

            Some(prev_sum)
        })
        .collect()
}
