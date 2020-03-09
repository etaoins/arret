#[derive(Debug)]
pub struct Workspace {
    name: String,
}

impl Workspace {
    pub fn new(name: String) -> Workspace {
        Workspace { name }
    }

    #[allow(dead_code)]
    pub fn name(&self) -> &str {
        &self.name
    }
}
